import re

def main():
    abbreviations = {}

    inputFile = open("AbbrevsFromFile.py", "r")
    inputString = inputFile.readlines()
    inputFile.close()

    wordsByFrequency = {}
    # Words which won't get acronyms, but kept around to make sure abbrevs won't conflict
    shortWords = {}
    for line in inputString:
        # Ignore comments
        if line.strip(" \t")[0] == '#':
            continue
        
        splitWords = line.split(" ")
        for word in splitWords:
            if word == '#':
                break
            
            # matches words and word.word2
            # 
            matches = re.findall(r'[_a-zA-Z][_a-zA-Z0-9.\-]+', word)
            for match in matches:
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
        
        print("----\n{}".format(word))
        """this-is_a_test2"""
        # TODO: This doesn't work for single letter words (e.g. the _a_)
        matches = re.findall(r"([a-zA-Z][a-z0-9]+)", word)
        # Tuples of (subword, index to next unused character in subword)
        subwords = []
        for match in matches:
            # print("subword {}".format(match))
            subwords.append([match, 1])
            
        # Ignore hungarian notation
        # TODO: Add support for 'pch' and the like
        # if len(word) == 1 or (word[0].isupper() or not word[1].isupper()):
        #     acronyms.append(word[0])

        # for i in range(1, len(word)):
        #     char = word[i]
        #     wordSeparators = ['.', '_', '-']
        #     if char.isupper() or word[i-1] in wordSeparators:
        #         acronyms.append(char)
        # print(acronyms)
        
        if not subwords:
            print("Error finding subwords for '{}'".format(word))
            continue

        # Try the minimum abbreviation first (e.g. only first letter), then rotate through
        abbreviationKey = subwords[0][0][0].lower()
        numSubwordsUsed = 1
        # Make sure that no abbreviation would conflict with a word in short words
        while abbreviationKey in abbreviations or abbreviationKey in shortWords:
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
                print("subword [{}] ({}) letters to use {} ({})"
                      .format(subwordIndex,
                              subwords[subwordIndex][0],
                              subwordLettersToUse,
                              subwords[subwordIndex][0][:subwordLettersToUse]))
                abbreviationKey += (subwords[subwordIndex][0][:subwordLettersToUse])
                
            abbreviationKey = abbreviationKey.lower()
            
        print("\tKey {} chosen".format(abbreviationKey))
        abbreviations[abbreviationKey] = word

    print(abbreviations)
    print("{} Abbreviations".format(len(abbreviations)))

if __name__ == '__main__':
    main()

    
# Local Variables:
# compile-command: "python3 AbbrevsFromFile.py"
# End: