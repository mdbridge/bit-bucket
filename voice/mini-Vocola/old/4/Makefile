default:
	compile *.mv

clean:
	rm -f  *.converted

run_convert:
	for i in *.xml ; do convert $$i; done

lists:
	(cd ~/Archive/JavaClient; make symbols)
	cp ~/Archive/JavaClient/symbols.txt Lists/my-java-types.list
	(cd ~/Elisp/Alist; get_work_email) > Lists/work-email.list
	extract_bookmarks bookmark.htm > Lists/my-web-sites.list
