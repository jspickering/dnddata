library(here)

set_file_wd = function(){
	command = commandArgs(trailingOnly = FALSE)

	file = gsub('--file=','',command[grepl('--file',command)])
	if(length(file) == 1){
		setwd(dirname(file))
	}
}
set_file_wd()

setwd(here())

library(git2r)
repo = repository(here('.'))
add(repo, 'data-raw/dnd_chars_all.tsv')
add(repo, 'README.md')
add(repo, 'README_files/*')
add(repo, 'data-raw/dnd_chars_unique.tsv')
add(repo, 'data-raw/dnd_chars_all.json')
add(repo, 'data-raw/dnd_chars_unique.json')

add(repo, 'data/*')


commit(repo, "auto update")
token = readLines(here('data-raw/auth'))
Sys.setenv(GITHUB_PAT = token)
cred = git2r::cred_token()
git2r::push(repo,credentials = cred)
