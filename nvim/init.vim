for f in split(glob('~/.files/nvim/config/*.vim'), '\n')
	exe 'source' f
endfor
