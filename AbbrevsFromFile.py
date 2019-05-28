import re
import argparse

debugPrint = False

commentChars = ['#', ';;', '//']

# Escape any characters in the word which would throw off the regex
def escapeRegexSymbols(word):
    return word.replace('.', '\\\\.')

# Build Emacs regular expression which highlights the abbreviation letters
def buildFontLockKeywords(word, abbreviation):
    fontLockKeywords = ''
    
    currentAbbrevIndex = 0
    lockOpen = False
    for char in word:
        if (currentAbbrevIndex < len(abbreviation)
            and char.lower() == abbreviation[currentAbbrevIndex].lower()):
            if not lockOpen:
                lockOpen = True
                fontLockKeywords += '\\\\('
            
            currentAbbrevIndex += 1
        else:
            if lockOpen:
                lockOpen = False
                fontLockKeywords+= '\\\\)'
                
        fontLockKeywords += char
    
        # In the case where the final char is part of the abbreviation, close it
    if lockOpen:
        lockOpen = False
        fontLockKeywords+= '\\\\)'
        
    return escapeRegexSymbols(fontLockKeywords)

outputBuffer = ''
def output(str):
    global outputBuffer
    if debugPrint:
        print(str)
    outputBuffer += str + '\n'

# Output in Emacs abbrev-file format
def outputAbbrevs(abbreviations):
    # print(""";;-*-coding: utf-8;-*-
    output("""(define-abbrev-table 'auto-abbrev-mode-abbrev-table
  '(""")
    
    for abbreviation, wordFontLockPair in abbreviations.items():
        output("    (\"{}\" \"{}\" nil :count 0)".format(abbreviation, wordFontLockPair[0]))
        
    output("   ))")

# For font lock output, sort in order of length so that more specific matches will
# overwrite less specific ones
def outputFontLockKeywords(abbreviations):
    output('''(setq auto-abbrev-highlights
      '(''')
    
    fontLockKeywords = []
    for abbreviation, wordFontLockPair in abbreviations.items():
        fontLockKeywords.append(wordFontLockPair[1])
        
    def sortByLenThenAlphabetical(str):
        return -len(str), str.lower()
    fontLockKeywords = sorted(fontLockKeywords, key = sortByLenThenAlphabetical)
    
    for keyword in fontLockKeywords:
        faces = "1 'auto-abbrev-highlight-face"
        numLocks = keyword.count('\\(')
        if numLocks > 1:
            faces = ''
            for faceNum in range(numLocks):
                faces += "({} 'auto-abbrev-highlight-face)".format(faceNum + 1)
        output("        (\"\\\\b{}\\\\b\" {})".format(keyword, faces))
        
    output("        ))")

def main():
    argparser = argparse.ArgumentParser()
    argparser.add_argument("-v", "--verbose", help = "Print verbose string parse output", action = "store_true")
    argparser.add_argument("inputFile", help = "The file to parse", type=str)
    argparser.add_argument("-o", "--outputFile", help = "The editor configuration file to output",
                           type=str, nargs = "?", default="auto-abbrevs-for-buffer.el")
    args = argparser.parse_args()
    if args.verbose:
        global debugPrint
        debugPrint = True
        
    print("Reading {}, outputting to {}".format(args.inputFile, args.outputFile))
    
    abbreviations = {}

    inputFile = open(args.inputFile, "r")
    inputString = inputFile.readlines()
    inputFile.close()

    wordsByFrequency = {}
    # Words which won't get acronyms, but kept around to make sure abbrevs won't conflict
    shortWords = {}
    for line in inputString:
        # Ignore comments
        if line.strip(" \t")[0] in commentChars:
            continue
        
        splitWords = line.split(" ")
        for word in splitWords:
            # Ignore subsequent words if the rest of the line is commented
            if word in commentChars:
                break
            
            # matches words and word.word2
            matches = re.findall(r'[_a-zA-Z][_a-zA-Z0-9.\-]+', word)
            for match in matches:
                if debugPrint:
                    print("word {} formatted {}".format(word, match))
            
                # Ignore things which aren't words (e.g. tabs)
                # Don't shorten already short words
                if len(match) < 4:
                    shortWords[match] = 1
                    continue
                if not match[0].isalnum():
                    continue
            
                if match in wordsByFrequency:
                    wordsByFrequency[match] += 1
                else:
                    wordsByFrequency[match] = 1
        
    wordsSortedByFrequency = sorted(wordsByFrequency.items(), key=lambda pair: pair[1], reverse=True)
    for wordPair in wordsSortedByFrequency:
        word = wordPair[0]
        acronyms = []
        
        if debugPrint:
            print("----\n{}".format(word))
            
        # TODO: This doesn't work for single letter words (e.g. the _a_)
        matches = re.findall(r"([a-zA-Z][a-z0-9]+)", word)
        # Tuples of (subword, index to next unused character in subword)
        subwords = []
        for match in matches:
            # print("subword {}".format(match))
            subwords.append([match, 1])
                    
        if not subwords:
            print("Error finding subwords for '{}'".format(word))
            continue

        # Try the minimum abbreviation first (e.g. only first letter), then rotate through
        abbreviationKey = subwords[0][0][0].lower()
        numSubwordsUsed = 1
        # Make sure that no abbreviation would conflict with a word in short words
        while abbreviationKey in abbreviations or abbreviationKey in shortWords:
            if debugPrint:
                print("\tKey {} failed".format(abbreviationKey))
            # This is true only after failing to use the first letter of all subwords
            # so that subword subsequent letters begin adding in
            allSubwordsUsed = False
            if numSubwordsUsed == len(subwords):
                allSubwordsUsed = True
            # Expand the number of words included
            elif numSubwordsUsed < len(subwords):
                numSubwordsUsed += 1

            # Construct the new abbreviation
            abbreviationKey = ''
            for subwordIndex in range(numSubwordsUsed):
                subwordLettersToUse = subwords[subwordIndex][1]
                # Start using the next letters in the word, if possible
                # TODO Only increment if less than the current min num required (e.g. add one letter
                # per iter, not one letter per subword per iter)
                if allSubwordsUsed and subwordLettersToUse < len(subwords[subwordIndex][0]):
                    subwordLettersToUse += 1
                    subwords[subwordIndex][1] = subwordLettersToUse
                if debugPrint:
                    print("subword [{}] ({}) letters to use {} ({})"
                          .format(subwordIndex,
                                  subwords[subwordIndex][0],
                                  subwordLettersToUse,
                                  subwords[subwordIndex][0][:subwordLettersToUse]))
                abbreviationKey += (subwords[subwordIndex][0][:subwordLettersToUse])
                
            abbreviationKey = abbreviationKey.lower()
            
        if debugPrint:
            print("\tKey {} chosen".format(abbreviationKey))
        fontLockKeywords = buildFontLockKeywords(word, abbreviationKey)
        abbreviations[abbreviationKey] = (word, fontLockKeywords)

    if abbreviations:
        outputAbbrevs(abbreviations)
        outputFontLockKeywords(abbreviations)
    
        outFile = open(args.outputFile, 'w')
        outFile.write(outputBuffer)
        outFile.close()
        
        if debugPrint:
            print(abbreviations)
        print("Done; created {} abbreviations".format(len(abbreviations)))
        
    else:
        print("Something didn't work. No abbreviations were created...")

if __name__ == '__main__':
    main()

    
# Local Variables:
# compile-command: "python3 AbbrevsFromFile.py"
# End:
