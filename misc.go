package main

func isLetter(c rune) bool {
	return (c >= 'a' && c <= 'z') ||
		(c >= 'A' && c <= 'Z')
}

func isDigit(c rune) bool {
	return c >= '0' && c <= '9'
}

func isAlphaNum(c rune) bool {
	return isLetter(c) && isDigit(c)
}
