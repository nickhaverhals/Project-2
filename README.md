# Project-2
Repo for the second project

In this project, I've created an app that allows users to investigate data I've collected from my students on the first day of class in the introductory Statistical Thinking course I've been teaching since Fall 2024. The questions I've asked are given below, along with, in brackets, the variable(s) used to record responses. 

1. How many pets do you have? [num_pets]

2. What kind of pet do you have? (Select all that apply.) [dog,cat, reptile,fish, bird,other,none]

3. How far (in miles) did you commute to get to class today? (For reference, if you live on campus, from Hill to Wick is 0.2 miles and from Shoemaker to Wick is 0.1 miles.) [dist]

4. How many minutes have you spent using your phone so far today? [minutes]

5. How old do you think your stats professor is? [how_old]

6. Were you born in the United States? [us_born]

7. How many letters are in the name you prefer your friends and family call you? [name_ltrs]

8. How many long has it been since you've seen a movie in a movie theater? [since_movie]

9. How many stuffed animals do you own? [num_stuffies]

10.Have you attended an institution besides SCSU after graduating high school (or a school equivalent to high school)? [non_scsu]

11.What year of university are you currently in? [stu_yr]

12. Randomly select a whole number between 1 and 10 (inclusive). [rand_num]

A few notes on data clean up. 

a. I also added columns for Semester (Fall or Spring) [variable: semester] and Academic Year (e.g., 2024/2025) [variable: acad_yr]. 

b. For question 9, I had a few outlandish answers: 10000000 and 98765432. The average of all other values was 7.5 stuffed animals so I replaced them with 7 and 8, respectively. 

c. Question 2 allowed for multiple selections so I created new columns to record whether students recorded, "Dog", "Cat", "Reptile", "Fish", "Bird", "Other", or 
"None" for question 2. I also added a "Primary pet" column which recorded the first pet listed but I think I won't use it, as it's misleading. The first pet listed was determined by the selection boxes, not necessarily what the student would consider their "primary" pet. So, that column is biased toward "Dog" and, although that's the correct answer, it likely doesn't represent all students' views faithfully. 



