import random

def guess_the_number(min_value, max_value):
    x = random.randint(min_value, max_value)
    name = input('What is your name?')
    guess = input('Guess a number between ' + str(min_value) + ' and ' + str(max_value) + ':')
    while int(guess) != x:
        if int(guess) < x:
            print('Too low!')
        elif int(guess) > x:
            print('Too high!')
        guess = input('Guess a number between ' + str(min_value) + ' and ' + str(max_value) + ':')
    print('Correct!')