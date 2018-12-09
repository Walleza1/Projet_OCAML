# Test files

## General comments

+ In all three test den, max flow is ensured as long as the number of spots available in the sum of all sports is greater than the number of students.
+ Every student must choose exctly 3 sports.
+ If we had a situation where we couldn't ensure that every student would have one of the three sports they picked, we could have automatically assigned
every students with sports they didnt pick with a really high cost.

## Graph 1

Functional test case, just to see if every student got their first choice as they should.

## Graph 2

Test case with a small number of students where everyone won't get their first choice.
+ Number of students : 4
+ Number of students with their 1st choice : 3
+ Number of students with their 2nd choice : 1
+ Number of students with their 3rd choice : 0

## Graph 3

Realistic test case with 40 students, 3 choices among a list of 5 sports, choices were randomized.
+ Number of students : 40
+ Number of students with their 1st choice : 37
+ Number of students with their 2nd choice : 3
+ Number of students with their 3rd choice : 0

