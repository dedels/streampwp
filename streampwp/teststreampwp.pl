:- module(teststreampwp, []).

%:- begin_tests(teststreampwp).

:- use_module(streampwp).

:- pwp_compile_files(relative(examplehtml), [export]).


%%staff(NickName, FullName, Office, Phone, E_Mail_Address).
staff(dave, 'David Edelstein', myoffice, 1234, 'email>@invalid.com').
staff(laura, 'Laura Gray', school, 4321, 'email2@i"nvalid<.com').
staff(max, 'Max Edelstein', gm, 9867, 'email3@something.com').

%%status(NickName, full_time | part_time).
status(dave, full_time).
status(laura, part_time).
status(max, full_time).




%:- end_tests(streampwp).
