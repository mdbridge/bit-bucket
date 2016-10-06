include "gnu.vch";
include "locale_Unix.vch";

Line(file) := EraseToStart() $file {enter};


## 
## Viewing MIME messages:
## 

include "URLs.vch";

ExportMIME() := Do(mdl-export-mime-to-l1);

  # munpack current buffer to ~/Tmp/export:
UnpackBuffer() := ExportMIME()
	          Shell("ruby ~/mail/unpack_l1.rb");


TypeRsync(source_machine, source_path, destination, which, options) :=
                  '"' PCfromPC(~pf32/cwRsync/bin/rsync.exe) '" '
		  ' -z -p $options '
                  '"$source_machine:$source_path/$which" '
		  '"/cygdrive/c/' 
                       Replace(Replace($destination, "\", "/") , "C:/", "") '"';

    Rsync(source_machine, source_path, destination, which, options) :=
                 ShellExecute(TypeRsync($source_machine, $source_path,
		 		        $destination, $which, $options));

CopyExport(location):=
	Rsync($location, ~/Tmp/export, PCfromPC(~/scratch/export), *, 
	      "-v -r --delete --progress")
	Wait(5000);

<location> := ( work=ts-rhel5.hpl.hp.com 
	      | personal=IfHome(mdl@192.168.1.2,mdl@foil.strangled.net) );

  # munpack current buffer to ~/Tmp/export then view that directory:
<location> [mime] unpack buffer =
	UnpackBuffer()
	CopyExport($1)
	AppBringUp("Export", "explorer /select," 
	                         PCfromPC(~/scratch/export/_contents_))
	Wait(2000)
	{ctrl+r}{ctrl+g};

<location> view HTML =
	UnpackBuffer()
	CopyExport($1)
	   # lookup text html part or whole message if none:
	Lookup(PCfromPC(~/scratch/export/_html.html));

<location> view meeting request =
	UnpackBuffer() Wait(1000)
	CopyExport($1)
	AppBringUp("meeting", PCfromPC(~/scratch/export/_meeting.ics));


type <location> command =
       TypeRsync($1, ~/Tmp/export, PCfromPC(~/scratch/export), *, 
	      "-v -n -r --delete --progress");
