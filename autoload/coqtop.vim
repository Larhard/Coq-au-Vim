" Vim autoload file for interacting with a Coq toplevel
" Maintainer: Emmanuel Beffara <manu@beffara.org>

" = Utility functions for positions and ranges = {{{1
"
" Positions are represented by pairs [line,column], where line is counted from
" 1 and column is a *byte* index counted from 1 (that is how Vim handles
" positions in getpos() and setpos()). Ranges are represented by pairs
" [first,last] with the positions of the first and last character in the
" range.

" Convert byte positions to character positions, relative to a given position.
" The byte numbers are expected to be in increasing order.

function! s:byte_pos (ref, bytes)
  let [l, c] = a:ref
  let pos = []
  let byte_ref = 0
  let line = strpart(getline(l), c - 1)
  for byte in a:bytes
    let byte -= byte_ref
    while byte >= strlen(line)
      let byte -= strlen(line) + 1
      let byte_ref += strlen(line) + 1
      let l += 1
      let c = 1
      let line = getline(l)
    endwhile
    if byte <= 0
      call add(pos, [l, c])
    else
      call add(pos, [l, c + byte - 1])
    endif
  endfor
  return pos
endfunction

" Compute the next position in the current buffer.

function! s:pos_next (p)
  let [l, c] = a:p
  if c == 0
    " Special case for the fake initial position [1,0]
    return [l, 1]
  endif
  let line = getline(l)
  if c < strlen(line)
    return [l, byteidx(line, strchars(strpart(line, 0, c - 1)) + 1) + 1]
  elseif l < line("$")
    return [l + 1, 1]
  else
    return [l, c]
  endif
endfunction

" Compute the previous position in the current buffer.

function! s:pos_prev (p)
  let [l, c] = a:p
  if c > 1
    let line = getline(l)
  else
    if l > 1
      let l -= 1
      let line = getline(l)
      let c = strlen(line) + 1
    else
      return [l, c]
    endif
  endif
  return [l, byteidx(line, strchars(strpart(line, 0, c - 1)) - 1) + 1]
endfunction

" Test if a given position is strictly before another one.

function! s:pos_lt (p, q)
  return a:p[0] < a:q[0] || (a:p[0] ==# a:q[0] && a:p[1] < a:q[1])
endfunction

" Get the text for a given range.

function! s:range_text (range)
  let [s, e] = a:range
  let lines = getline(s[0], e[0])
  let lines[-1] = strcharpart(lines[-1], 0,
    \ strchars(strpart(lines[-1], 0, e[1] - 1)) + 1)
  let lines[0] = strpart(lines[0], s[1] - 1)
  return join(lines, "\n")
endfunction


" = XML handling = {{{1
"
" The following functions handle a simple subset of XML that is sufficient for
" interaction with the Coq top-level. They are not a full-featured XML parser.
"
" XML nodes are represented as follows:
" - an element node is a triple [tag, attr; contents] where 'tag' is the tag
"   name (as a string), 'attr' is a dictionary of attributes and 'contents' is
"   the contents of the node, as a list of nodes,
" - a text node is a string.
" Character entities are part of the XML encoding, they are not present in
" the data structures.

" == String escaping == {{{2

function! s:entity_escape (text)
  return substitute(a:text, "[&'\"<>]", {m -> s:char2entity[m[0]]}, 'g')
endfunction

let s:char2entity = {
\ '&': '&amp;',
\ "'": '&apos;',
\ '"': '&quot;',
\ '<': '&lt;',
\ '>': '&gt;' }

function! s:entity_unescape (text)
  return substitute(a:text, '&\(#\?[a-z0-9]\+\);',
    \ '\=s:entity_chr(submatch(1))', 'g')
endfunction

let s:entity2char = {
\ 'amp': '&',
\ 'apos': "'",
\ 'quot': '"',
\ 'lt': '<',
\ 'nbsp': ' ',
\ 'gt': '>' }

function! s:entity_chr (name)
  if a:name[0] !=# '#'
    return get(s:entity2char, a:name, '&' . a:name . ';')
  elseif a:name[1] ==# 'x'
    return nr2char('0' . a:name[1:])
  else
    return nr2char(a:name[1:])
  endif
endfunction


" == XML formatting == {{{2
"
" The function xml_format turns structured data into a string holding the
" corresponding XML. The argument is a list of nodes, each node that is not a
" list is turned into a string of character data.

function! s:xml_format (items)
  let output = ''
  for item in a:items
    if type(item) !=# v:t_list
      let output .= s:entity_escape(item)
      continue
    endif

    let [tag, attr; contents] = item
    let output .= '<' . tag
    for [key, value] in items(attr)
      let output .= ' ' . key . '="' . s:entity_escape(value) . '"'
    endfor
    if empty(contents)
      let output .= '/>'
    else
      let output .= '>' . s:xml_format(contents) . '</' . tag . '>'
    endif
  endfor
  return output
endfunction


" == XML parsing == {{{2
"
" The function xml_parse parses a possibly unterminated chunk of XML text. The
" first argument is the text. The second argument is a stack of open nodes:
" each item is a valid  node that has been open by previous parsing operations
" and not closed yet. The return value is a list of completed items (strings
" or nodes) and the stack argument is modified to hold the new parsing state.
" The stack ends up empty when all open tags have been closed.
"
" The function expects that the input text contains complete tags, for
" instance '<tag attr="foo' is considered an error and will be treated like
" '<tag attr="foo">'.

function! s:xml_parse (xml, stack)
  let output = []
  let len = strlen(a:xml)
  let pos = 0
  while pos < len

    " Find the next tag.

    let tag_pos = stridx(a:xml, '<', pos)
    if tag_pos ==# -1
      let tag_pos = len
    endif

    " Handle the string that precedes the tag.

    if tag_pos > pos
      let string = s:entity_unescape(strpart(a:xml, pos, tag_pos - pos))
      if empty(a:stack)
        call add(output, string)
      else
        call add(a:stack[-1], string)
      endif
    endif
    if tag_pos ==# len
      break
    endif

    " Find the tag name.

    let tag_pos += 1
    if strpart(a:xml, tag_pos, 1) ==# '/'
      let opening = v:false
      let closing = v:true
      let tag_pos += 1
    else
      let opening = v:true
      let closing = v:false
    endif
    let pos = match(a:xml, '[ >/]', tag_pos)
    if pos <= tag_pos
      throw "coqtop:xml:empty_tag(" . tag_pos . ")"
      let pos = tag_pos
    endif
    let tag = strpart(a:xml, tag_pos, pos - tag_pos)
    let attr = {}

    " Parse the attributes.

    while pos < len
      let pos = match(a:xml, '[^ ]', pos)
      if pos ==# -1
        throw "coqtop:xml:open_tag(" . pos . ")"
        let pos = len
      endif
      if strpart(a:xml, pos, 1) ==# '>'
        let pos += 1
        break
      endif
      if strpart(a:xml, pos, 1) ==# '/'
        let closing = v:true
        let pos = stridx(a:xml, '>', pos + 1)
        if pos ==# -1
          throw "coqtop:xml:unterminated_tag"
          let pos = len
        else
          let pos += 1
        endif
        break
      endif
      let end_pos = match(a:xml, '[=>/]', pos)
      if end_pos ==# -1
        throw "coqtop:xml:unterminated_tag"
        let pos = end_pos
        break
      endif
      let key = strpart(a:xml, pos, end_pos - pos)
      if strpart(a:xml, end_pos, 1) ==# '='
        if strpart(a:xml, end_pos + 1, 1) ==# '"'
          let pos = end_pos + 2
          let end_pos = stridx(a:xml, '"', pos)
          if end_pos ==# -1
            throw "coqtop:xml:unterminated_string(" . pos . ")"
            let end_pos = len
          endif
          let attr[key] =
            \ s:entity_unescape(strpart(a:xml, pos, end_pos - pos))
          let pos = end_pos + 1
        else
          let pos = end_pos + 1
          let end_pos = match(a:xml, '[ >]', pos)
          if end_pos ==# -1
            let end_pos = len
          endif
          let attr[key] =
            \ s:entity_unescape(strpart(a:xml, pos, end_pos - pos))
          let pos = end_pos
        endif
      else
        let attr[key] = v:none
        let pos = end_pos
      endif
    endwhile

    " Update the stack.

    if opening
      call add(a:stack, [tag, attr])
    endif

    if closing
      let top = remove(a:stack, -1)
      if top[0] !=# tag
        throw "coqtop:xml:invalid_closing_tag(" . tag_pos . "," . top[0] . "," . tag . ")"
      endif
      if empty(a:stack)
        call add(output, top)
      else
        call add(a:stack[-1], top)
      endif
    endif
  endwhile

  return output
endfunction


" == Matching data structures == {{{2
"
" The function Match matches a value against a pattern. The pattern is
" interpreted literally except that:
"   - a string '$name' matches any value, with the name 'name',
"   - a string '*name' in a list matches all the remaining elements of the
"     value,
"   - a string '?name' in a dictionary accepts the case when the value does
"     not contain the key,
"   - a dictionary is matched key by key, each key in the pattern must be
"     present in the value (except with '?name' patterns) but other keys may
"     be present too,
"   - a list ['?'; choices] is interpreted as a choice: the liste 'choices'
"     consists of pairs [pattern,dict] where 'pattern' is tried against the
"     value and 'dict' contains extra items for the result value; the first
"     match in the list of choices will provide the result.
" The returned value is a dictionary mapping names to matched values in case
" of success, or v:none in case of failure.
"
" This function acts on generic data structures, but we use it here to extract
" information from parsed XML.

function! s:match (value, pattern)
  " String patterns.

  if type(a:pattern) ==# v:t_string
    if a:pattern[0] ==# '$'
      " The pattern is a key.
      return {a:pattern[1:]: a:value}
    elseif type(a:value) ==# v:t_string && a:value ==# a:pattern
      " The pattern is an explicit string, the value matches.
      return {}
    else
      " The pattern is an explicit string, the value does not match.
      return v:none
    endif
  endif

  " Choice patterns

  if type(a:pattern) ==# v:t_list && !empty(a:pattern)
  \ && type(a:pattern[0]) ==# v:t_string && a:pattern[0] ==# '?'
    return s:match_first(a:value, a:pattern[1:], {})
  endif

  " In all other cases, the types must match.

  if type(a:value) !=# type(a:pattern)
    return v:none
  endif

  " Matching lists.

  if type(a:pattern) ==# v:t_list
    let result = {}
    let i = 0

    while i < len(a:pattern)
      let pat = a:pattern[i]
      if type(pat) ==# v:t_string && pat[0] ==# '*'
        " The pattern matches all remaining value nodes.
        let result[pat[1:]] = a:value[i:]
        return result
      endif

      if i >= len(a:value)
        " The value does not have enough values
        return v:none
      endif

      " Recursively match a node.
      let r = s:match(a:value[i], pat)
      if type(r) ==# v:t_none
        return v:none
      endif
      for [key, match_val] in items(r)
        let result[key] = match_val
      endfor

      let i += 1
    endwhile

    if len(a:value) > i
      " The value has too many elements.
      return v:none
    endif

    return result
  endif

  " Matching dictionaries.

  if type(a:pattern) ==# v:t_dict
    let result = {}

    for [key, pat] in items(a:pattern)
      if type(pat) ==# v:t_string && pat[0] ==# '?'
        if has_key(a:value, key)
          let result[pat[1:]] = a:value[key]
        endif
        continue
      endif

      if !has_key(a:value, key)
        " An expected key is missing.
        return v:none
      end

      let r = s:match(a:value[key], pat)
      if type(r) ==# v:t_none
        return v:none
      endif
      for [key, match_val] in items(r)
        let result[key] = match_val
      endfor
    endfor

    return result
  endif

  " Other types are not supported.

  throw "coqtop:match:invalid_arguments"
  return v:none
endfunction

" Match a value against a list of patterns and return the dictionary for the
" forst match. The arguments 'patterns' is a list of pairs [pattern, dict]
" where 'pattern' is used as for the 'Match' function and 'dict' is a
" dictionary of extra items added to the return value in case of match. An
" optional third argument specifies extra items to add in the return value.

function! s:match_first (value, patterns, ...)
  let dict = a:0 > 0 ? a:1 : {}
  for [pattern, extra] in a:patterns
    let r = s:match(a:value, pattern)
    if type(r) !=# v:t_none
      for [k, v] in items(dict) + items(extra)
        let r[k] = v
      endfor
      return r
    endif
  endfor
  return v:none
endfunction


" = Communication with Coq = {{{1
"
" Everything related to Coq interaction is encapsulated in an object called
" b:coq. This is a dictionary that contains all the relevant state as well as
" methods for interaction with coqtop (using Vim's mechanism of dictionary
" functions). The script object s:coq is a template that gets instanciated
" as a buffer object b:coq when a session is started, then everything goes
" through this buffer-local object.
"
" The prover state, seen from Vim, consists in the following data:
"   - states: a dictionary mapping state IDs to their descriptions
"   - focus: the state ID of the current sentence in focus
"   - zones: a set of text zones (see 'highlighting')
"   - last_pos: the start position of the last range sent to Coq (between a
"     call to Add and the reply, used to locate potential error messages)
"
" Each state is described by the following data:
"   - range: the position of the corresponding sentence in the buffer
"   - parent: the id of the parent state [optional]
"   - next: the id of the child state [optional]
"   - status: either 'added' or 'checked'
"   - messages: a list of messages received as feedback
" For the initial state, the range is set to [[1,0], [1,0]].

let s:coq = { }

" The default logging function does nothing. It is overridden when debugging.

function s:coq.log (...)
endfunction

" == Job management and basic communication == {{{2

" Coqtop interacts asynchronously by exchanging messages as XML nodes. We use
" Vim8's jobs and channels for interaction with a callback to handle received
" messages. Messages are of three kinds: feedback, message and value. There is
" one value per command and the way it is handled depends on the command that
" it responds to. For this reason, we maintain a list of handlers for the next
" expected values.

" Start the job for coqtop.

function s:coq.start_job ()
  if has_key(self, 'job') && !empty(self.job)
    call self.stop_job()
  endif
  let cmd = 'coqtop -ideslave -main-channel stdfds -async-proofs on'
  call self.log('j', "start", cmd)
  let self.job = job_start(cmd, {'mode': 'raw', 'out_cb': self.callback})
  let self.channel = job_getchannel(self.job)
  call self.log('j', "started", self.job, self.channel)
  let self.xml_stack = []
  let self.return_queue = []
endfunction

" Stop the running job.

function s:coq.stop_job ()
  call self.log('j', "stop", self.job)
  call job_stop(self.job)
  let self.job = v:none
endfunction

" Send a method call to coqtop. The 'argument' is structured data according to
" the representation of XML as described above. The function returns after
" sending the request, the 'callback' will be called asynchronously when the
" answer eventually arrives.
"
" The extra arguments are pairs [pattern, extra] such that if the answer is
" good and matches 'pattern', the callback is called with a dictionary of
" matches from the pattern, augmented with the items in 'dict' and in 'extra'.
"
" In any case, the dictionary contains at least a field 'success' that is true
" if the call succeeded and false on failure.

function s:coq.call (command, argument, callback, dict, ...)
  let xml = s:xml_format([['call', {'val': a:command}, a:argument]])
  call self.log('x', "send", xml)
  call add(self.return_queue, [a:command, a:000, a:callback, a:dict])
  call ch_sendraw(self.channel, xml)
endfunction

" Main callback for messages from coqtop.

let s:coq_answer_pattern =[
  \ [['value', {'val': 'good'},
  \   '*content'],
  \ {'type': 'good'}],
  \ [['value', {'val': 'fail', 'loc_s': '?start', 'loc_e': '?end'},
  \   ['state_id', {'val': '$state_id'}],
  \   ['richpp', {}, '$message']],
  \ {'type': 'fail'}],
  \ [['message', {},
  \   ['message_level', {'val': '$level'}],
  \   ['?',
  \     [['option', {'val': 'none'}], {}],
  \     [['option', {'val': 'some'},
  \       ['loc', {'start': '$start', 'stop': '$end'}]], {}]],
  \   ['richpp', {}, '$message']],
  \ {'type': 'message'}],
  \ [['feedback', {},
  \   ['?',
  \     [['state_id', {'val': '$state_id'}], {}],
  \     [['edit_id', {'val': '$edit_id'}], {}] ],
  \   ['feedback_content', {'val': '$kind'}, '*content']],
  \ {'type': 'feedback'}] ]

function s:coq.callback (channel, msg)
  call self.log('x', "recv", a:msg)
  for item in s:xml_parse(a:msg, self.xml_stack)
    call self.log('x', "item", item)
    let r = s:match_first(item, s:coq_answer_pattern)
    if type(r) ==# v:t_none
      call self.protocol_error("Message not understood:", item)
      continue
    endif
    call self.log('m', 'message', r)

    " Return value for a request.

    if r.type ==# 'good' || r.type ==# 'fail'
      if empty(self.return_queue)
        call self.protocol_error("Unexpected value:", item)
        continue
      endif
      let [cmd, formats, Callback, dict] = remove(self.return_queue, 0)
      let dict.success = (r.type ==# 'good')
      if r.type ==# 'good'
        let r = s:match_first(r.content, formats, dict)
        if type(r) == v:t_none
          call self.protocol_error("Invalid answer for", cmd, ":", item)
          continue
        endif
        call self.log('r', "good", r)
      else
        for [k,v] in items(dict)
          let r[k] = v
        endfor
        call self.log('r', "fail", r)
      endif
      call Callback(r)

    " Old style informational message.

    elseif r.type ==# 'message'
      call self.print_message(r,
        \ has_key(self, 'last_pos') ? self.last_pos : v:none)

    " Feedback.

    elseif r.type ==# 'feedback'
      " Feedback for edit IDs is ignored since we do not use edit IDs here.
      if has_key(r, 'state_id')
        call self.feedback(r.state_id, r.kind, r.content)
      endif

    endif
  endfor
endfunction

" A function called on protocol errors.

function s:coq.protocol_error (...)
  if self.debugging
    call function(self.log, ['e', "protocol"] + a:000)()
  else
    echohl WarningMsg
    echomsg "Coq protocol error (run in debugging mode for details)"
    echohl None
  endif
  throw "coqtop:protocol:error"
endfunction


" == Coq commands == {{{2
"
" For each request, we provide a function that expects an argument for each
" field in the request, plus a callback (named 'return') and a dictionary
" 'dict' of extra parameters. When an answer is received, 'return' is called
" with a dictionary containg the conents of 'dict' plus the fields of the
" answer, always with at leas a field 'success' that indicates whether the
" call succeeded.
"
" We provide one function per call in the Coq API, on return the state
" representation is updated.

" about() {{{3
" -> {version, protocol, release, compile}
"
" Get information avout Coq: version number, protocol version, release date
" and compile date.

function s:coq.call_about (return, dict)
  call self.log('r', "call", "about")
  call self.call('About',
    \ ['unit', {}],
    \ a:return, a:dict,
    \ [[['coq_info', {},
    \   ['string', {}, '$version'],
    \   ['string', {}, '$protocol'],
    \   ['string', {}, '$release'],
    \   ['string', {}, '$compile']]],
    \ {}])
endfunction

" add(range, edit_id, state_id, verbose) {{{3
" -> {state_id, message, closing, [next_state_id], range, parent_id}
"
" Add a new state for a given range of text in the buffer, as successor of
" 'state_id' and with identifier 'edit_id' (unused here). If 'verbose' is true
" then informational messages will be received about the new state. In the
" return value, 'closing' is true when the sentence closes an existing focus,
" then 'next_state_id' is the ID of the new focus.

function s:coq.call_add (range, edit_id, state_id, verbose, return, dict)
  let text = s:range_text(a:range)
  call self.log('r', "call", "add",
    \ a:range, a:edit_id, a:state_id, a:verbose, text)
  let self.last_pos = a:range[0]
  let a:dict.range = a:range
  call self.call('Add',
    \ ['pair', {},
    \   ['pair', {},
    \     ['string', {}, text],
    \     ['int', {}, a:edit_id]],
    \   ['pair', {},
    \     ['state_id', {'val': a:state_id}],
    \     ['bool', {'val': a:verbose ? 'true' : 'false'}]]],
    \ function(self.return_add, [a:return]), a:dict,
    \ [[['pair', {},
    \   ['state_id', {'val': '$state_id'}],
    \   ['pair', {},
    \     ['?',
    \       [['union', {'val': 'in_l'},
    \         ['unit', {}]],
    \       {'closing': v:false}],
    \       [['union', {'val': 'in_r'},
    \         ['state_id', {'val': '$next_state_id'}]],
    \       {'closing': v:true}] ],
    \     ['string', {}, '*message']]]],
    \ {'parent_id': a:state_id}])
  call self.highlight(a:range, 'CoqSent')
endfunction

function s:coq.return_add (return, d)
  unlet self.last_pos
  if !a:d.success
    call self.highlight(a:d.range, '')
    return a:return(a:d)
  endif
  call self.highlight(a:d.range, 'CoqAdded')
  let self.states[a:d.parent_id].next = a:d.state_id
  if a:d.closing
    " A subproof is closed.
    let s = self.states[a:d.state_id]
    let s.range = a:d.range
    let s.parent = a:d.parent_id
    let self.focus = a:d.next_state_id
  else
    " A new state is created.
    let self.states[a:d.state_id] = {
      \ 'range': a:d.range,
      \ 'parent': a:d.parent_id,
      \ 'status': 'added',
      \ 'messages': [] }
    let self.focus = a:d.state_id
  endif
  call a:return(a:d)
endfunction

" edit_at(state_id) {{{3
" -> {zone, [start_id, end_id, old_id], states}
"
" Set a new edit point. In the return value, 'zone' is true when editing in a
" delimited zone, then 'start_id' indicates the 'Proof.' sentence and 'end_id'
" indicates the corresponding 'Qed.' sentence. The 'states' field contains
" the sentences that were deleted.

function s:coq.call_edit_at (state_id, return, dict)
  call self.log('r', "call", "edit_at", a:state_id)
  call self.call('Edit_at',
    \ ['state_id', {'val': a:state_id}],
    \ function(self.return_edit_at, [a:return]), a:dict,
    \ [[['union', {'val': 'in_l'}, ['unit', {}]]],
    \ {'zone': v:false, 'id': a:state_id}],
    \ [[['union', {'val': 'in_r'}, ['pair', {},
    \   ['state_id', {'val': '$start_id'}],
    \   ['pair', {},
    \     ['state_id', {'val': '$end_id'}],
    \     ['state_id', {'val': '$old_id'}]]]]],
    \ {'zone': v:true, 'id': a:state_id}])
endfunction

function s:coq.return_edit_at (return, d)
  if !a:d.success
    return a:return(a:d)
  endif

  let a:d.states = []
  let s = self.states[a:d.id]
  while has_key(s, 'next')
    let id = remove(s, 'next')
    if a:d.zone && id ==# a:d.end_id
      unlet self.states[id].parent
      break
    endif
    let s = remove(self.states, id)
    call self.highlight(s.range, '')
    call add(a:d.states, s)
  endwhile

  let self.focus = a:d.id
  call a:return(a:d)
endfunction

" get_options() {{{3
" -> {options: [{name, synchronous, deprecated, infos, type, value}, ...]}
"
" Get the state of all options in the current session.

function s:coq.call_get_options (return, dict)
  call self.log('r', "call", "get_options")
  call self.call('GetOptions',
    \ ['unit', {}],
    \ function(self.return_get_options, [a:return]), a:dict,
    \ [[['list', {}, '*options']], {}])
endfunction

function s:coq.return_get_options (return, d)
  if !a:d.success
    return a:return(a:d)
  endif
  let options = []
  for item in a:d.options
    let r = s:match(item, ['pair', {},
      \ ['list', {}, '*name'],
      \ ['option_state', {},
      \   ['bool', {'val': '$synchronous'}],
      \   ['bool', {'val': '$deprecated'}],
      \   ['string', {}, '$info'],
      \   ['?',
      \     [['option_value', {'val': 'boolvalue'},
      \       ['bool', {'val': '$value'}]],
      \     {'type': 'bool'}],
      \     [['option_value', {'val': 'intvalue'},
      \       ['?',
      \         [['option', {'val': 'none'}],
      \         {'value': v:none}],
      \         [['option', {'val': 'some'}, ['int', {}, '$value']],
      \         {}]]],
      \     {'type': 'int'}],
      \     [['option_value', {'val': 'stringvalue'},
      \       ['string', {}, '*value']],
      \     {'type': 'string'}],
      \     [['option_value', {'val': 'stringoptvalue'},
      \       ['?',
      \         [['option', {'val': 'none'}],
      \         {'value': v:none}],
      \         [['option', {'val': 'some'}, ['string', {}, '$value']],
      \         {}]]],
      \     {'type': 'stringopt'}]]]])

    if type(r) ==# v:t_none
      call self.protocol_error("Option state:", item)
      continue
    endif
    let name = []
    for word in r.name
      let s = s:match(word, ['string', {}, '$word'])
      if type(s) ==# v:t_none
        call self.protocol_error("Option name word:", word)
        continue
      endif
      call add(name, s.word)
    endfor
    let r.name = name
    let r.synchronous = r.synchronous ==# 'true'
    let r.deprecated = r.deprecated ==# 'true'
    if r.type ==# 'bool'
      let r.value = r.value ==# 'true'
    elseif r.type ==# 'string'
      let r.value = join(r.value)
    endif
    call add(options, r)
  endfor
  let a:d.options = options
  call a:return(a:d)
endfunction

" goal() {{{3
" -> {[current, background, shelved, abandoned]}
"
" Get the list of goals in the current state. The return value is empty when
" there is no proof open.

function s:coq.call_goal (return, dict)
  call self.log('r', "call", "goal")
  call self.call('Goal',
    \ ['unit', {}],
    \ a:return, a:dict,
    \ [[['option', {'val': 'none'}]], {}],
    \ [[['option', {'val': 'some'}, ['goals', {},
    \   ['list', {}, '*current'],
    \   ['list', {}, '*background'],
    \   ['list', {}, '*shelved'],
    \   ['list', {}, '*abandoned'] ]]], {}])
endfunction

" init() {{{3
" -> {state_id}
"
" Initialise the prover. This must be called first and once. The return value
" is the identifier of the initial state.

function s:coq.call_init (return, dict)
  call self.log('r', "call", "init")
  call self.call('Init',
    \ ['option', {'val': 'none'}],
    \ function(self.return_init, [a:return]), a:dict,
    \ [[['state_id', {'val': '$state_id'}]], {}])
endfunction

function s:coq.return_init (return, d)
  if !a:d.success
    return a:return(a:d)
  endif

  let id = a:d.state_id
  let self.states = {id: {
    \ 'range': [[1, 0], [1, 0]],
    \ 'status': 'checked',
    \ 'messages': [] }}
  let self.focus = a:d.state_id
  let self.zones = []

  call a:return(a:d)
endfunction

" query(text, state_id) {{{3
" -> {message}
"
" Perform a query in some state.

function s:coq.call_query (text, state_id, return, dict)
  call self.log('r', "call", "query", a:text, a:state_id)
  call self.call('Query',
    \ ['pair', {},
    \   ['string', {}, a:text],
    \   ['state_id', {'val': a:state_id}]],
    \ a:return, a:dict,
    \ [[['string', {}, '*message']], {}])
endfunction

" quit() {{{3
" -> {}
"
" Terminate the Coq session.

function s:coq.call_quit (return, dict)
  call self.log('r', "call", "quit")
  call self.call('Quit',
    \ ['unit', {}],
    \ a:return, a:dict,
    \ [[['unit', {}]], {}])
endfunction

" set_options([{name, type, value}, ...]) {{{3
" -> {}
"
" Set some options in the Coq session. Each option is represented as a
" dictionary with the following fields:
"  - name : the option name as a list of strings
"  - type : the type of the option, as a string among bool, int, string, stringopt
"  - value : the requested value (v:none is mapped to none for option types)

function s:coq.call_set_options (options, return, dict)
  call self.log('r', "call", "set_options", a:options)
  let items = []
  for item in a:options
    if item.type ==# 'bool'
      let coded = ['bool', {'val': item.value ? 'true' : 'false'}]
    elseif item.type ==# 'int'
      if type(item.value) ==# v:t_none
        let coded = ['option', {'val': 'none'}]
      else
        let coded = ['option', {'val': 'some'}, ['int', {}, item.value + 0]]
      endif
    elseif item.type ==# 'string'
      let coded = ['string', {}, item.value]
    elseif item.type ==# 'stringopt'
      if type(item.value) ==# v:t_none
        let coded = ['option', {'val': 'none'}]
      else
        let coded = ['option', {'val': 'some'}, ['string', {}, item.value]]
      endif
    else
      throw "coqtop:call_set_options:invalid_arguments"
    endif
    call add(items, ['pair', {},
      \ ['list', {}] + map(item.name, {i, s -> ['string', {}, s]}),
      \ ['option_value', {'val': item.type . 'value'}, coded]])
  endfor
  call self.call('SetOptions',
    \ ['list', {}] + items,
    \ a:return, a:dict,
    \ [[['unit', {}]], {}])
endfunction


" == Handling feedback == {{{2

function s:coq.feedback(state_id, kind, content)
  if !has_key(self.states, a:state_id)
    return self.log('e', 'feedback', 'feedback for unknown state', a:state_id)
  endif

  let r = s:match_first([a:kind, a:content], [
    \ [['processed', []], {}],
    \ [['complete', []], {}],
    \ [['message', [['message', {},
    \   ['message_level', {'val': '$level'}],
    \   ['?',
    \     [['option', {'val': 'none'}], {}],
    \     [['option', {'val': 'some'},
    \       ['loc', {'start': '$start', 'stop': '$end'}]], {}]],
    \   ['richpp', {}, '$message']]]], {}],
    \ ])

  if a:kind ==# 'processed'
    call self.log('f', a:kind, a:state_id)
    if !has_key(self.states, a:state_id)
      return
    endif
    let state = self.states[a:state_id]
    if state.status !=# 'checked'
      call self.highlight(state.range, 'CoqChecked')
      let state.status = 'checked'
    endif

  elseif a:kind ==# 'complete'
    call self.log('f', a:kind, a:state_id)

  elseif a:kind ==# 'message'
    let r = s:match(a:content, [['message', {},
      \ ['message_level', {'val': '$level'}],
      \ ['?',
      \   [['option', {'val': 'none'}], {}],
      \   [['option', {'val': 'some'},
      \     ['loc', {'start': '$start', 'stop': '$end'}]], {}]],
      \ ['richpp', {}, '$message']]])
    if type(r) ==# v:t_none
      return self.protocol_error("feedback message", a:content)
    endif
    call self.log('f', 'message', a:state_id, r)
    call add(self.states[a:state_id].messages, r)

  else
    call self.log('f', a:kind, a:state_id, a:content)
  endif
endfunction


" == Highlighting regions == {{{2
"
" The field 'zone' in a coq object is a list of currently highlighted zones.
" Its items are dictionaries with the following entries:
"   - range: the range, in the format [[ls,cs], [le,cs]]
"   - group: the highlighting group
"   - match: the match ID as provided by matchadd()
" As an invariant, all zones are disjoint and adjacent zones have different
" groups.

" Clear the zones. The optional argument is group name, if it is used then
" only the zones with this name will be cleared.

function s:coq.clear_zones (...)
  if a:0 ==# 0
    for z in self.zones
      call self.drop_zone(z)
    endfor
    let self.zones = []
  else
    let new = []
    for z in self.zones
      if z.group ==# a:1
        call self.drop_zone(z)
      else
        call add(new, z)
      endif
    endfor
    let self.zones = new
  endif
endfunction

" Update a zone. This accepts a dictionary, possibly empty, and returns it
" updated.

function s:coq.update_zone (zone, range, group)
  call self.log('h', 'update', a:zone, a:range, a:group)

  let [ls, cs] = a:range[0]
  let [le, ce] = a:range[1]

  let pat = '\%' . ls . 'l'
  if cs > 1
    let pat .= '\%>' . (cs - 1) . 'c'
  endif
  if le > ls + 1
    let pat .= '\|\%>' . ls . 'l\%<' . le . 'l'
  endif
  if le > ls
    let pat .= '\|\%' . le . 'l'
  endif
  let pat .= '\%<' . (ce + 1) . 'c'

  if has_key(a:zone, 'match')
    call matchdelete(a:zone.match)
    let a:zone.match = matchadd(a:group, pat, 10, a:zone.match)
  else
    let a:zone.match = matchadd(a:group, pat)
  endif
  let a:zone.range = a:range
  let a:zone.group = a:group

  return a:zone
endfunction

" Remove a zone from the highlighter.

function s:coq.drop_zone (zone)
  call self.log('h', 'drop', a:zone)
  call matchdelete(a:zone.match)
endfunction

" Set the group for a given range. If the specified group is empty, the range
" is removed from the highlighter.

function s:coq.highlight (range, group)
  let [s, e] = a:range

  if e ==# [1, 0]
    " The fake initial range is ignored.
    return
  endif

  let s1 = s:pos_prev(s)
  let e1 = s:pos_next(e)
  let new = []
  let fresh = {}

  for z in self.zones
    let [rs, re] = z.range
    if z.group ==# a:group
      if s:pos_lt(re, s1) || s:pos_lt(e1, rs)
        " The range is strictly before or after the new one, keep it unchanged.
        call add(new, z)
        continue
      endif
      " The range gets extended.
      if s:pos_lt(rs, s)
        let s = rs
        let s1 = s:pos_prev(s)
      endif
      if s:pos_lt(e, re)
        let e = re
        let e1 = s:pos_next(e)
      endif
      if empty(fresh)
        let fresh = z
      else
        call self.drop_zone(z)
      endif

    else
      " The zone is in a different group.
      if s:pos_lt(re, s) || s:pos_lt(e, rs)
        " The range is before or after what is removed.
        call add(new, z)
        continue
      endif

      if s:pos_lt(rs, s)
        " The beginning of the zone is preserved.
        call add(new, self.update_zone(z, [rs, s1], z.group))
        if s:pos_lt(e, re)
          " The end of the zone is preserved too.
          call add(new, self.update_zone({}, [e1, re], z.group))
        endif
      elseif s:pos_lt(e, re)
        " The end of the zone is preserved.
        call add(new, self.update_zone(z, [e1, re], z.group))
      else
        " The zone is completely removed.
        call self.drop_zone(z)
      endif
    endif
  endfor

  if !empty(a:group)
    call self.update_zone(fresh, [s, e], a:group)
    call add(new, fresh)
  endif
  let self.zones = new
endfunction


" = User interface = {{{1

" == Interface windows == {{{2
"
" We use a prototype object 's:window' to encapsulate operations related to
" writing to other windows. The main coq object will contain an instance for
" each window: goals, infos and possibly a debug window.

let s:window = {}

" Create a new window with a new bufffer using a given name and filetype. The
" buffer will have no file associated to it, a number may be added to the
" buffer name to ensure it is unique. The third argument specifies whether
" splitting should be vertical or horizontal.

function s:window.create (name, type, vertical)
  if a:vertical
    rightbelow vnew
  else
    rightbelow new
  endif
  if bufnr(a:name) < 0
    let name = a:name
  else
    let i = 1
    while v:true
      let name = a:name . ' [' . i . ']'
      if bufnr(name) < 0
        break
      endif
      let i += 1
    endwhile
  endif
  silent execute 'file ' . name
  setlocal buftype=nofile
  setlocal noswapfile
  execute 'setlocal filetype=' . a:type
  let new = copy(self)
  let new.id = win_getid()
  let new.buffer = bufnr("")
  return new
endfunction

" Delete a window and its buffer.

function s:window.close ()
  execute 'bdelete! ' . self.buffer
endfunction

" Clear the contents of a window.

function s:window.clear ()
  let win = win_getid()
  call win_gotoid(self.id)
  0,$d _
  call win_gotoid(win)
endfunction

" Write text to a window. The argument is either a string or a list of
" strings. The '\n' character is interpreted and each string is split into
" lines.

function s:window.write (text)
  let win = win_getid()
  call win_gotoid(self.id)
  call append(line('$') - 1, a:text)
  normal! G
  call win_gotoid(win)
endfunction


" == Decoding rich messages == {{{2
"
" Decode a pretty-printed message from Coq and return a list of text lines.
" This function simply strips all tags and preserves only textual content, a
" future version may use the information to do something more intelligent.

function! s:richpp_format (data)
  return split(s:richpp_format_unsplit(a:data), '\n', v:true)
endfunction

function! s:richpp_format_unsplit (data)
  if type(a:data) ==# v:t_string
    return a:data
  endif
  let text = ""
  for item in a:data[2:]
    let text .= s:richpp_format_unsplit(item)
  endfor
  return text
endfunction

" Properly display a message in the 'infos' window. The 'pos' argument is a
" position in the buffer, used when the messages contains a location (as a
" byte offset).

function s:coq.print_message(message, pos)
  if a:message.level ==# 'error' && has_key(a:message, 'start')
    call self.highlight(s:byte_pos(a:pos, [a:message.start, a:message.end]), 'CoqError')
  endif
  call self.infos.write(s:richpp_format(a:message.message))
endfunction


" == Opening and closing sessions == {{{2

" Create a session with its window and a coqtop job.
"
" When the job has started, we check that the protocol version is supported
" and we make an initial state.

function s:coq.create ()
  let new = copy(self)
  let new.debugging = exists('b:coq_debug')

  let winid = win_getid()
  let new.goals = s:window.create("Goals", '', v:true)
  let new.infos = s:window.create("Infos", '', v:false)
  if new.debugging
    let new.debug = s:window.create("Log", '', v:false)
    syntax region ErrorMsg start="^e|" end="$"
    let new.log = new.log_debug
  endif
  call win_gotoid(winid)

  call new.start_job()
  call new.call_about(new.start_version, {})
  return new
endfunction

function s:coq.start_version (d)
  if a:d.protocol !=# '20150913'
    return self.infos.write("Unsupported Coq protocol version: "
      \ . a:d.protocol)
  endif
  call self.infos.write("Coq version "
    \ . a:d.version . " (" . a:d.release . ") started.")
  call self.call_init(self.start_state, {})
endfunction

function s:coq.start_state (d)
  call self.infos.write("Ready.")
endfunction

" Terminate a session and close the windows.

function s:coq.quit ()
  try
    call self.call_quit(self.quit_return, {})
  endtry
endfunction

function s:coq.quit_return (d)
  if !a:d.success
    return
  endif
  call self.stop_job()
  call self.goals.close()
  call self.infos.close()
  if self.debugging
    call self.debug.close()
  endif
  call self.clear_zones()
  call coqtop#Quit()
endfunction

" Log function when debugging.

function s:coq.log_debug (kind, head, ...)
  if stridx(b:coq_debug, a:kind) < 0
    return
  endif
  if stridx(b:coq_debug, 't') >= 0
    let s = strftime('[%H:%M:%S] ')
  else
    let s = ''
  endif
  let s = printf('%s%s|%-12s|', s, a:kind, a:head)
  let quote = v:false
  for arg in a:000
    if type(arg) ==# v:t_none
      let quote = v:true
      continue
    endif
    if type(arg) !=# v:t_string
      let arg = string(arg)
    endif
    if quote
      let arg = '"'
        \ . tr(substitute(arg, "[\\\n\t]", '\\&', 'g'), "\n\t", "nt") . '"'
      let quote = v:false
    endif
    let s .= " " . arg
  endfor
  call self.debug.write(s)
endfunction

" == Showing goals == {{{2

function s:coq.show_goals ()
  call self.call_goal(self.show_goals_return, {})
endfunction

function s:coq.show_goals_return (d)
  if !a:d.success
    return self.call_edit_at(a:d.state_id, self.rewind_return, {})
  endif
  let win = win_getid()
  call win_gotoid(self.goals.id)
  0,$d _
  if has_key(a:d, 'current')
    if empty(a:d.current)
      call append(line('$') - 1, "No goal")
      let pos = getpos('$')
    else
      call s:append_goal(a:d.current[0], v:true)
      let pos = getpos('$')
      for goal in a:d.current[1:]
        call self.goals.write(["", repeat('=', 78), ""])
        call s:append_goal(goal, v:true)
      endfor
    endif
    if !empty(a:d.background) || !empty(a:d.shelved) || !empty(a:d.abandoned)
      call self.goals.write("")
      if !empty(a:d.background)
        call self.goals.write("Background: " . len(a:d.background))
      endif
      if !empty(a:d.shelved)
        call self.goals.write("Shelved: " . len(a:d.shelved))
      endif
      if !empty(a:d.abandoned)
        call self.goals.write("Abandoned: " . len(a:d.abandoned))
      endif
    endif
    call setpos('.', pos)
    normal z-
  endif
  call win_gotoid(win)
endfunction

function! s:append_goal (goal, long)
  let r = s:match(a:goal, ['goal', {},
    \ ['string', {}, '$name'],
    \ ['list', {}, '*hyp'],
    \ '$goal' ])
  if type(r) ==# v:t_none
    call append(line('$') - 1, ["Invalid structure:", string(a:goal)])
  else
    if a:long
      for item in r.hyp
        call append(line('$') - 1, s:richpp_format(item))
      endfor
      call append(line('$') - 1, repeat('-', 78))
    else
      call append(line('$') - 1, "")
    endif
    call append(line('$') - 1, s:richpp_format(r.goal))
  endif
endfunction


" == Main commands == {{{2

" Send the next sentence.

function s:coq.send_next ()
  try
    call self.infos.clear()
    call self.clear_zones('CoqError')
    let initial = getcurpos()
    let s = self.states[self.focus]
    let start = s:pos_next(s.range[1])
    call setpos('.', [0] + start)
    if CoqNextPeriod(v:true)
      call self.call_add([start, getpos('.')[1:2]], -1, self.focus, v:true,
        \ self.send_next_return, {})
    endif
    call setpos('.', initial)
  endtry
endfunction

function s:coq.send_next_return (d)
  if !empty(a:d.message)
    let a:d.level = a:d.success ? 'info' : 'error'
    call self.print_message(a:d, a:d.range[0])
  endif
  if a:d.success
    call self.show_goals()
  else
    call self.call_edit_at(a:d.state_id, self.rewind_return, {})
  endif
endfunction

" Rewind one sentence.

function s:coq.rewind ()
  try
    call self.infos.clear()
    call self.clear_zones('CoqError')
    let s = self.states[self.focus]
    if !has_key(s, 'parent')
      call self.log('r', "rewind", "no parent for", self.focus)
      return
    endif
    call self.call_edit_at(s.parent, self.rewind_return, {})
  endtry
endfunction

function s:coq.rewind_return (d)
  if a:d.success
    for s in a:d.states
      let pos = s.range[0]
      for m in s.messages
        call self.print_message(m, pos)
      endfor
    endfor
  endif
  call self.show_goals()
endfunction

" Add or rewind until a given point.

function s:coq.to_cursor ()
  try
    call self.infos.clear()
    call self.clear_zones('CoqError')
    let pos = getpos('.')[1:2]
    if s:pos_lt(self.states[self.focus].range[1], pos)
      " The current position is after the focus, add sentences.
      " Fake a return code from call_add to initialise the loop.
      call self.send_until_loop({
        \ 'success': v:true, 'closing': v:false, 'state_id': self.focus,
        \ 'message': [], 'end': pos })
    else
      " The current position is before the focus, rewind to the last state
      " before the current position.
      let last_id = v:none
      let last_pos = [0, 0]
      for [id, st] in items(self.states)
        if s:pos_lt(st.range[1], pos) && s:pos_lt(last_pos, st.range[1])
          let last_id = id
          let last_pos = st.range[1]
        end
      endfor
      call assert_notequal(type(last_id), v:t_none)
      call self.call_edit_at(last_id, self.rewind_return, {})
    endif
  endtry
endfunction

function s:coq.send_until_loop (d)
  " On failure, stop and edit where the error occurred.
  if !a:d.success
    return self.call_edit_at(a:d.state_id, self.rewind_return, {})
  endif

  if !empty(a:d.message)
    call self.infos.write("***" . string(a:d.message))
  endif

  " On success, search for the next sentence.
  let initial = getcurpos()
  let id = a:d.closing ? a:d.next_state_id : a:d.state_id
  let s = self.states[id]
  let start = s:pos_next(s.range[1])
  call setpos('.', [0] + start)
  if CoqNextPeriod(v:false)
    let pos = getpos('.')[1:2]
    if s:pos_lt(pos, a:d.end)
      " There is a next sentence before the goal position, add it.
      call setpos('.', initial)
      return self.call_add([start, pos], -1, id, v:true,
        \ self.send_until_loop, {'end': a:d.end})
    endif
  endif

  " The end position has been reached.
  call setpos('.', initial)
  call self.show_goals()
endfunction

" Send a query for the focussed state.

function s:coq.query (text)
  try
    call self.infos.clear()
    call self.call_query(a:text, self.focus, self.query_return, {})
  endtry
endfunction

function s:coq.query_return (d)
  if a:d.success
    call self.infos.write(a:d.message)
  else
    call self.infos.write(s:richpp_format(a:d.message))
  endif
endfunction


" == Queries == {{{2

" Send a Query command made from the text of the current selection. The first
" argument is the current mode (see :map-operator in the Vim documentation)
" and the second argument is a printf template for the query.

function! s:operator (mode, format)
  if a:mode ==? 'v'
    let range = [getpos("'<")[1:2], getpos("'>")[1:2]]
  else
    let range = [getpos("'[")[1:2], getpos("']")[1:2]]
  endif
  call b:coq.log('r', 'operator', a:mode, range, a:format)
  call b:coq.query(printf(a:format, s:range_text(range)))
endfunction

" The following functions are 'operatorfunc's for particular queries.

function! s:op_about (mode)
  try
    call s:operator(a:mode, 'About %s.')
  endtry
endfunction

function! s:op_check (mode)
  try
    call s:operator(a:mode, 'Check %s.')
  endtry
endfunction

function! s:op_search_about (mode)
  try
    call s:operator(a:mode, 'SearchAbout %s.')
  endtry
endfunction


" == Options == {{{2

" When the argument list is empty, show the whole list of options in the info
" window, in a minimalistic readable way, otherwise set some options.
"
" The arguments have the shape Option_Name:type=value where
" the option name has spaces replaced by underscores, type is among bool, int,
" string, stringopt, and the value is interpreted accordingly. If the type is
" omitted, bool is assumed. If the value part is omitted, a default value is
" used (true for bool, none for int and stringopt, empty for string).

function s:coq.set_options (...)
  try
    if a:0 ==# 0
      return self.call_get_options(self.show_options_return, {})
    endif
    let items = []
    for arg in a:000
      " Decompose the option into name, type and value

      let p1 = stridx(arg, ':')
      if p1 < 0
        let p2 = stridx(arg, '=')
        if p2 < 0
          let opt = {'name': arg}
        else
          let opt = {'name': strpart(arg, 0, p2), 'value': strpart(arg, p2 + 1)}
        endif
      else
        let opt = {'name': strpart(arg, 0, p1)}
        let p2 = stridx(arg, '=', p1 + 1)
        if p2 < 0
          let opt.type = strpart(arg, p1 + 1)
        else
          let opt.type = strpart(arg, p1 + 1, p2 - p1 - 1)
          let opt.value = strpart(arg, p2 + 1)
        endif
      endif

      let opt.name = split(opt.name, '_')

      " Fill in default types and values

      if !has_key(opt, 'type')
        let opt.type = 'bool'
      endif
      if opt.type ==# 'bool'
        if !has_key(opt, 'value')
          let opt.value = v:true
        elseif index(['true', '1', 'yes'], opt.value) >= 0
          let opt.value = v:true
        elseif index(['false', '0', 'no'], opt.value) >= 0
          let opt.value = v:false
        else
          throw "coqtop:set_options:invalid_arguments"
        endif
      elseif opt.type ==# 'int' || opt.type ==# 'stringopt'
        if !has_key(opt, 'value')
          let opt.value = v:none
        endif
      elseif opt.type ==# 'string'
        if !has_key(opt, 'value')
          let opt.value = ""
        endif
      else
        throw "coqtop:set_options:invalid_arguments"
      endif

      call add(items, opt)
    endfor

    call self.call_set_options(items, self.set_options_return, {})
  endtry
endfunction

function s:coq.show_options_return (d)
  if a:d.success
    let lines = []
    for opt in a:d.options
      call add(lines, printf("%s : %s = %s [%s%s] %s",
        \ join(opt.name), opt.type, string(opt.value),
        \ (opt.synchronous ? 'S' : ''), (opt.deprecated ? 'D' : ''),
        \ opt.info))
    endfor
    call self.infos.clear()
    call self.infos.write(lines)
  else
    call self.infos.write(s:richpp_format(a:d.message))
  endif
endfunction

function s:coq.set_options_return (d)
  if a:d.success
    call self.show_goals()
  else
    call self.infos.write(s:richpp_format(a:d.message))
  endif
endfunction


" == Commands and mappings == {{{2

function! coqtop#Start ()
  let b:coq = s:coq.create()
  command! -buffer CoqQuit :call b:coq.quit()
  command! -buffer CoqNext :call b:coq.send_next()
  command! -buffer CoqRewind :call b:coq.rewind()
  command! -buffer CoqToCursor :call b:coq.to_cursor()
  command! -buffer -nargs=1 CoqQuery :call b:coq.query("<args>")
  command! -buffer -nargs=* CoqSet :call b:coq.set_options(<f-args>)

  nnoremap <buffer> <silent> <C-Down>  :CoqNext<CR>
  nnoremap <buffer> <silent> <C-Up>    :CoqRewind<CR>
  nnoremap <buffer> <silent> <C-Right> :CoqToCursor<CR>

  inoremap <buffer> <silent> <C-Down>  <C-\><C-o>:CoqNext<CR>
  inoremap <buffer> <silent> <C-Up>    <C-\><C-o>:CoqRewind<CR>
  inoremap <buffer> <silent> <C-Right> <C-\><C-o>:CoqToCursor<CR>

  nnoremap <buffer> <silent> <localleader>a :set opfunc=<sid>op_about<cr>g@
  vnoremap <buffer> <silent> <localleader>a :<C-U>call <sid>op_about(visualmode())<cr>
  nnoremap <buffer> <silent> <localleader>c :set opfunc=<sid>op_check<cr>g@
  vnoremap <buffer> <silent> <localleader>c :<C-U>call <sid>op_check(visualmode())<cr>
  nnoremap <buffer> <silent> <localleader>s :set opfunc=<sid>op_search_about<cr>g@
  vnoremap <buffer> <silent> <localleader>s :<C-U>call <sid>op_search_about(visualmode())<cr>

  if b:coq.debugging
    nnoremap <buffer> K :CoqQuit<CR>
    nnoremap <buffer> C :message clear<CR>
    nnoremap <buffer> S :CoqStart<CR>
    execute 'nnoremap L :source ' . s:my_source . '<CR>'
  endif
endfunction

let s:my_source = expand('<sfile>:p')

function! coqtop#Quit ()
  delcommand CoqQuit
  delcommand CoqNext
  delcommand CoqRewind
  delcommand CoqToCursor
  delcommand CoqQuery
  delcommand CoqSet

  nunmap <buffer> <C-Down>
  nunmap <buffer> <C-Right>
  nunmap <buffer> <C-Up>

  iunmap <buffer> <C-Down>
  iunmap <buffer> <C-Right>
  iunmap <buffer> <C-Up>

  nunmap <buffer> <localleader>a
  vunmap <buffer> <localleader>a
  nunmap <buffer> <localleader>c
  vunmap <buffer> <localleader>c
  nunmap <buffer> <localleader>s
  vunmap <buffer> <localleader>s

  unlet b:coq
endfunction

hi link CoqError Error

if &background ==# 'light'
  hi default CoqSent    guibg=#ffeedd ctermbg=lightred
  hi default CoqAdded   guibg=#ffddee ctermbg=yellow
  hi default CoqChecked guibg=#eeffdd ctermbg=lightgreen
else
  hi default CoqSent    guibg=#661100 ctermbg=darkred
  hi default CoqAdded   guibg=#664400 ctermbg=brown
  hi default CoqChecked guibg=#113300 ctermbg=darkgreen
endif
