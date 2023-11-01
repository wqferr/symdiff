local dir = (...):match [[(.+)%.?[^%.]*]]
return require(dir..".symdiff")