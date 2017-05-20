# web-crawler

Requirements:

1.	Follow the basic crawling algorithm provided in the slides
2.	Crawl 50 pages for your repository
3.	Only store websites which contain have at least one term in the body text from a list of keywords chosen by your group
4.	Store the following information in a character vector: “<Page Title> <URL>”

Error checking requirements:
1.	If the link in the frontier is a link to a jpg, go to the next item in the frontier
2.	If the retrieved page is less than 10 characters, go to the next item in the frontier
3.	Check for relative/absolute paths when adding to the frontier
4.	You may come across other implementation challenges during testing
