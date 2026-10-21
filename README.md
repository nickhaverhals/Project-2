# Project-2

In this project, I've created an app that allows users to investigate data I've collected from my students on the first day of class in the introductory Statistical Thinking course I've been teaching since Fall 2024. The questions I've asked are given below, along with, in brackets, the variable(s) used to record responses. 

__Helpful tips__

To use the app, begin by selecting two categorical and two numerical variables. For the categorical variables, you can select particular values to look at (subset on). For the numerical variables, you can select a range of values for each (again to subset the data). The default for each will be to select all responses for each variable. 



__Questions and associate variables__

1. How many pets do you have? ["Number of pets" = `num_pets`]

2. What kind of pet do you have? (Select all that apply.) ["Pet owner (any)" = `dog`, `cat`, `reptile`, `fish`, `bird`,`other`, `none`]

3. How far (in miles) did you commute to get to class today? (For reference, if you live on campus, from Hill to Wick is 0.2 miles and from Shoemaker to Wick is 0.1 miles.) ["Distance traveled to class" = `dist`]

4. How many minutes have you spent using your phone so far today? ["Time on phone in minutes" = `minutes`]

5. How old do you think your stats professor is? ["Guess at professor's age" = `how_old`]

6. Were you born in the United States? [Native to US = `us_born`]

7. How many letters are in the name you prefer your friends and family call you? ["Number of letters in nickname" = `name_ltrs`]

8. How many long has it been since you've seen a movie in a movie theater? [Time since going to the cinema = `since_movie`]

9. How many stuffed animals do you own? ["Number of stuffed animals" = `num_stuffies`]

10. Have you attended an institution besides SCSU after graduating high school (or a school equivalent to high school)? [Transfer status = `non_scsu`]

11. What year of university are you currently in? [University class level = `stu_yr`]

12. Randomly select a whole number between 1 and 10 (inclusive). ["Randomly selected number" = `rand_num`]

Additional variables: 

I also added columns for Semester (Fall or Spring) [`semester`] and Academic Year (e.g., 2024/2025) [`acad_yr`]. 

__A few notes on data clean up__

a. For question 9, I had a few outlandish answers: 10000000 and 98765432. The average of all other values was 7.5 stuffed animals so I replaced them with 7 and 8, respectively. 

b. Question 2 allowed for multiple selections so I created new columns to record whether students recorded, "Dog", "Cat", "Reptile", "Fish", "Bird", "Other", or 
"None" for question 2. I also added a "Primary pet" column which recorded the first pet listed but I think I won't use it, as it's misleading. The first pet listed was determined by the order of selection boxes, not necessarily what the student would consider their "primary" pet. So, that column is biased toward "Dog" and, although that's the correct answer, it likely doesn't represent all students' views faithfully. 


