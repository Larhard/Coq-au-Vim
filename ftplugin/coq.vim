" = Motions = {{{1

" Search forward for a pattern, skipping comments. The return value is the
" number of the first maching sub-expression (as for the Vim function search()
" with the 'p' option). The second argument indicates whether a match at the
" current position is accepted.

function! s:SearchForward (pattern, immediate)
  let immediate = a:immediate
  while v:true
    let result = search('\((\*\)\|\(\*)\)\|' . a:pattern,
      \ immediate ? 'cepWz' : 'epWz')
    if result < 2
      return result
    elseif result > 3
      return result - 2
    elseif result ==# 3
      " A closing comment delimiter was found, ignore it.
      let immediate = v:false
      continue
    endif
    " A comment was found, skip it and retry.
    let level = 1
    while level > 0
      let result = search('(\*\|\(\*)\)', 'epWz')
      if result ==# 0
        " A comment is left open at the end of the buffer.
        return 0
      elseif result ==# 1
        " An opening delimiter was found.
        let level += 1
      else
        " A closing delimiter was found.
        let level -= 1
      endif
    endwhile
    let immediate = v:false
  endwhile
endfunction

" Search backward for a pattern, skipping comments. The return value is the
" number of the first maching sub-expression (as for the Vim function search()
" with the 'p' option). The second argument indicates whether a match at the
" current position is accepted.

function! s:SearchBackward (pattern, immediate)
  let immediate = a:immediate
  while v:true
    let result = search('\((\*\)\|\(\*)\)\|' . a:pattern,
      \ immediate ? 'bcpW' : 'bpW')
    if result < 2
      return result
    elseif result > 3
      return result - 2
    elseif result ==# 2
      " An opening comment delimiter was found, ignore it.
      let immediate = v:false
      continue
    endif
    " A comment was found, skip it and retry.
    let level = 1
    while level > 0
      let result = search('(\*\|\(\*)\)', 'bpW')
      if result ==# 0
        " A comment is left open at the beginning of the buffer.
        return 0
      elseif result ==# 1
        " An opening delimiter was found.
        let level -= 1
      else
        " A closing delimiter was found.
        let level += 1
      endif
    endwhile
    let immediate = v:false
  endwhile
endfunction

" Check if the current position is a beginning of sentence, i.e. if it is
" located between a period or bullet and the next non-blank character. The
" argument indicates how the cursor is considered:
" - if true, a period or bullet under the cursor is accepted but the first
"   non-blank character of a sentence is refused,
" - if false, the cursor position is accepted even if it contains the first
"   non-blank character of a sentence.

function! s:IsBeginningOfSentence (immediate)
  let immediate = a:immediate
  let position = getpos(".")
  while v:true
    let result = s:SearchBackward('\.\_s\|\([-+*{}]\)\|\(\S\)', immediate)
    if result <= 1
      " A period or the beginning of the buffer was found.
      call setpos(".", position)
      return v:true
    elseif result ==# 2
      " A potential bullet was found, validate it by checking it is a
      " beginning of sentence.
      let immediate = v:false
      continue
    else
      " Something else was found before.
      call setpos(".", position)
      return v:false
    endif
  endwhile
endfunction

" Move to the next period or bullet. Return v:true if one was found, v:false
" otherwise (i.e. when the end of the buffer was reached). The argument
" indicates whether a match at the current position is accepted.

function! CoqNextPeriod (immediate)
  let immediate = a:immediate
  while v:true
    let result = s:SearchForward('\.\_s\@=\|\([-+*{}]\)', immediate)
    if result ==# 2
      " A potential bullet was found, check it.
      if s:IsBeginningOfSentence(v:false)
        return v:true
      endif
    else
      return result !=# 0
    endif
    let immediate = v:false
  endwhile
endfunction

" Move to the previous period or bullet. Return v:true if one was found,
" v:false otherwise (i.e. when the beginning of the buffer was reached).

function! CoqPreviousPeriod (immediate)
  let immediate = a:immediate
  while v:true
    let result = s:SearchBackward('\.\_s\|\([-+*{}]\)', immediate)
    if result ==# 2
      " A potential bullet was found, check it.
      if s:IsBeginningOfSentence(v:false)
        return v:true
      endif
    else
      return result !=# 0
    endif
    let immediate = v:false
  endwhile
endfunction

" Move to the next sentence. The 'count' argument indicates how many sentences
" to advance. Return v:true if a next sentence was found, v:false otherwise.

function! CoqNextSentence (count)
  if !s:IsBeginningOfSentence(v:true)
    call CoqNextPeriod(v:true)
  endif
  let i = a:count
  while i > 1
    if !CoqNextPeriod(v:false)
      return v:false
    endif
    let i -= 1
  endwhile
  return s:SearchForward('\S', v:false) !=# 0
endfunction

" Move the the beginning of the current sentence, or the previous one if the
" cursor is already at the beginning of a sentence. The 'count' argument
" indicates how many sentences to advance. Return v:true if a proper previous
" sentence was found, v:false otherwise.

function! CoqPreviousSentence (count)
  if s:IsBeginningOfSentence(v:false)
    call CoqPreviousPeriod(v:false)
  endif
  let i = a:count
  while i > 1
    if !CoqPreviousPeriod(v:false)
      return v:false
    endif
    let i -= 1
  endwhile
  return s:SearchForward('\S', !CoqPreviousPeriod(v:false)) !=# 0
endfunction

" Select a sentence in visual mode.

function! CoqVisualSentence (inner)
  let [l, c] = getpos(".")[1:2]
  if s:IsBeginningOfSentence(v:true)
    \ && stridx(' .-+*', strcharpart(getline(l), c - 1, 1)) <= 0
    " The cursor is on a blank between a period and the next sentence:
    " - if inner is true, select the blanks between the period ans the
    "   sentence,
    " - if inner is false, select also the next sentence.
    if CoqPreviousPeriod(v:false)
      call s:SearchForward('\s', v:false)
    endif
    " call setpos("'<", getpos("."))
    normal v
    if a:inner
      call s:SearchForward('\s\S\@=', v:true)
    else
      call CoqNextPeriod(v:false)
    endif
  else
    " The cursor is in a sentence. Select the sentence, plus the following
    " space if inner is false.
    if CoqPreviousPeriod(v:false)
      call s:SearchForward('\S', v:false)
    endif
    " call setpos("'<", getpos("."))
    normal v
    call CoqNextPeriod(v:true)
    if !a:inner
      call s:SearchForward('\s\S\@=', v:true)
    endif
  endif
  call setpos("'>", getpos("."))
endfunction

" = Folding = {{{1

function! CoqFoldLevel (lnum)
	let line = getline(a:lnum)
	if line =~ "(\\*\\* \\*\\+ "
		return ">1"
	elseif line =~ "^  \\*{2,} "
		return ">1"
	elseif line ==# "Proof."
		return ">2"
	elseif line =~# "^\\(Qed\\|Defined\\|Abort\\|Admitted\\)."
		return "<2"
	endif
	return "="
endfunction

function! CoqFoldText ()
	let text = getline(v:foldstart)
	if text =~ "\\((\\*\\* \\|  \\)\\*\\+ "
		if strpart(text, 0, 4) ==# "(** "
			let text = strpart(text, 4)
		else
			let text = strpart(text, 2)
		endif
		let text = substitute(text, " *\\(\\*)\\)\\?$", "", "")
		let level = stridx(text, " ")
		let text = "--[ " . strpart(text, level + 1) . " ]"
		while level > 1
			let text = "---" . text
			let level = level - 1
		endwhile
		return printf("%s-- %s lines ", text, v:foldend - v:foldstart)
	else
		return printf("-- %d lines: %s ", v:foldend - v:foldstart, text)
	endif
endfunction


" = Commands and mappings = {{{1

command! -buffer CoqStart :call coqtop#Start()
nnoremap <buffer> <silent> ( :<C-U>call CoqPreviousSentence(v:count)<CR>
nnoremap <buffer> <silent> ) :<C-U>call CoqNextSentence(v:count)<CR>
vnoremap <buffer> <silent> is :<C-U>call CoqVisualSentence(v:true)<CR>
vnoremap <buffer> <silent> as :<C-U>call CoqVisualSentence(v:false)<CR>

setlocal shiftwidth=2 tabstop=2 expandtab
setlocal foldexpr=CoqFoldLevel(v:lnum) foldtext=CoqFoldText()
