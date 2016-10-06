RUBY    = ruby

all: whos_where.rol whos_where_email whos_where.csv \
	evite.rol personal-phones.txt \
	to_evite.csv to_phone.csv personal_email.txt


### 
### Generating/maintaining Rolodexes:
### 

ROLODEX = code/rolodex.rb


## 
## Allowing human editing of personal addresses (addresses.rol):
## 

show: 
	ruby code/cleanup.rb addresses.rol > addresses.rol.clean
	diff addresses.rol addresses.rol.clean

step: 
	ruby code/cleanup.rb addresses.rol > addresses.rol.clean
	${RUBY} code/backup.rb addresses.rol
	-diff addresses.rol addresses.rol.clean
	cp addresses.rol addresses.rol.old
	mv  addresses.rol.clean addresses.rol

clean::
	rm -f addresses.rol.old addresses.rol.clean


## 
## Extracting information from scraped evite page:
## 

evite.rol: evite.htm    ${ROLODEX} code/import_evite.rb
	$(RUBY) code/import_evite.rb < evite.htm > $@

clean::
	rm -f evite.rol



### 
### Using Rolodexes:
### 

## 
## Extracting list of personal emails:
## 

personal_email.txt: ${ROLODEX} code/get_personal_email.rb addresses.rol
	${RUBY} code/get_personal_email.rb > $@

clean::
	rm -f personal_email.txt


## 
## Generating phone lists:
## 

PHONE_LIST = code/phone_list.rb     ${ROLODEX}

personal-phones.txt: addresses.rol     ${PHONE_LIST}
	$(RUBY) code/phone_list.rb < addresses.rol > $@

clean::
	rm -f personal-phones.txt


## 
## Dealing with Florey sub-Rolodex/mailing list:
## 

whos_where.rol whos_where_email whos_where.csv: addresses.rol code/florey.rb ${ROLODEX}
	$(RUBY) code/florey.rb
	rm -f temporary

clean::
	rm -f whos_where.rol whos_where_email whos_where.csv
	rm -f temporary


## 
## Generate CSV file of selected people for use in uploading to web
## sites like evite:
## 

to_evite.csv: addresses.rol   code/export_evite.rb ${ROLODEX}
	ruby code/export_evite.rb

clean::
	rm -f to_evite.csv


## 
## Generate CSV file of selected people for use in setting up a phone's contacts:
## 

to_phone.csv: addresses.rol   code/export_phone.rb ${ROLODEX}
	ruby code/export_phone.rb

clean::
	rm -f to_phone.csv
