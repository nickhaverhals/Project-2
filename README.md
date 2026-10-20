# Project-2
Repo for the second project

In this project, I've created an app that allows users to investigate data I've collected from my students on the first day of class in the introductory Statistical Thinking course I've been teaching since Fall 2024. The questions I've asked are: 

1. How many pets do you have?

2. What kind of pet do you have? (Select all that apply.)

3. How far (in miles) did you commute to get to class today? (For reference, if you live on campus, from Hill to Wick is 0.2 miles and from Shoemaker to Wick is 0.1 miles.)

4. How many minutes have you spent using your phone so far today?

5. How old do you think your stats professor is?

6. Were you born in the United States?

7. How many letters are in the name you prefer your friends and family call you?

8. How many long has it been since you've seen a movie in a movie theater?

9. How many stuffed animals do you own?

10.Have you attended an institution besides SCSU after graduating high school (or a school equivalent to high school)?

11.What year of university are you currently in?

12. Randomly select a whole number between 1 and 10 (inclusive).

A few notes on data clean up. 

a. For question 9, I had a few outlandish answers: 10000000 and 98765432. The average of all other values was 7.5 stuffed animals so I replaced them with 7 and 8, respectively. 

b. Question 2 allowed for multiple selections so I created new columns to record whether students recorded, "Dog", "Cat", "Reptile", "Fish", "Bird", "Other", or 
"None". I also added a "Primary pet" column which recorded the first pet listed but I think I won't use it, as it's misleading. The first pet listed was determined by the selection boxes, not necessarily what the student would consider their "primary" pet. So, that column is biased toward "Dog" and, although that's the right answer, it likely doesn't represent students' views faithfully. 

c. Finally, I added columns for Semester (Fall or Spring) and Academic Year (e.g., 2024/2025). 

