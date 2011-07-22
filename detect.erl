-module(detect).
-compile(export_all).
-define(MAX_BYTES_TO_READ,1024*100).


getType(FileName) 		->	detect ( file:open(FileName,[read,binary]), file:open("magic.mime",[read,binary])  ).
							
detect({error,_},_)				->	"File does not exist.";
detect(_,{error,_})				->	"MagicFile does not exist.";
detect({ok,IoFile},{ok,IoMagic})		->	{ok, FileData}   = file:read(IoFile , ?MAX_BYTES_TO_READ),
							file:close(IoFile),
							MagicList = parse_all_lines(IoMagic, []),
							compare(FileData,MagicList).
										 
										

parse_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> parse_all_lines(Device, Accum ++ parse(Line))
    end.




%Checks whether a given digit is available for Decimal , Hexadecimal and Octal or not.
isDecDigit(Char)	-> 	(Char >= $0) and (Char =< $9).
isOctDigit(Char)	-> 	(Char >= $0) and (Char =< $7).
isHexDigit(Char)	->	(Char >= $0) and (Char =< $9) or   
				(Char >= $a) and (Char =< $f) or 
				(Char >= $A) and (Char =< $F).



%This functions takes binary type as argument and returns if it is Decimal, Hex or Octal number.
isDec(<<>>)	->	false;
isDec(Var)	->	<< <<X>> || <<X>> <= Var , isDecDigit(X)  >> == Var.
isHex(<<>>)	->	false;
isHex(Var)	->	<< <<X>> || <<X>> <= Var , isHexDigit(X)  >> == Var.
isOct(<<>>)	->	false;
isOct(Var)	->	<< <<X>> || <<X>> <= Var , isOctDigit(X)  >> == Var.

%This functions takes binary type as argument and converts it into Decimal from Hexadecimal,Octal,Decimal.
hex2int(Hex) ->
    erlang:list_to_integer((erlang:binary_to_list(Hex)), 16).
    
oct2int(Hex) ->
    erlang:list_to_integer((erlang:binary_to_list(Hex)),  8). 
       
dec2int(Hex) ->
    erlang:list_to_integer((erlang:binary_to_list(Hex)), 10).


%This function converts hex,oct or dec (e.g. 0xaef32 , 01242 , 3563) to decimal.
hdo2int(<<$0 , $x, T/binary>>)	-> hex2int(T);
hdo2int(<<$0 , T/binary>>    )	-> oct2int(T);
hdo2int(T)		        -> dec2int(T).


parse(<<$>	,_/binary>>)	->	[]; % gecici olarak duruyor silinecek.
parse(<<$#	,_/binary>>)	->	[];
parse(<<$\n	,_/binary>>)	->	[];
parse({{_,false},_,_,_})	->	[];
parse({_,{_,false},_,_})	->	[];		
parse({_,_,{_,false},_})	->	[];	
parse({{Offset,true},{Type,true},{Data,true},Result})	->	[edit({Offset,Type,Data,Result})];	
parse(Binary)				->	{Offset,ExOffset} = getOffset(Binary,  <<>>)  ,%essential line parsing start
						{Type  ,ExType}   = getType(ExOffset,  <<>>)  , 
						{Data  ,ExData}   = getData(ExType  ,  <<>>)  ,
						Result    = getResult(ExData)  	   ,
						parse({	{Offset,isValidOffset(Offset)	},
							{Type  ,isValidType(Type,<<>>)	},
							{Data  ,isValidData(Type,Data)	},
						        Result				   }).
								 


getOffset(<<>> ,Offset)			-> {Offset,<<>>};
getOffset(<<$\n, T/binary>> ,Offset)	-> {Offset,T};
getOffset(<<$\s, T/binary>> ,Offset)	-> {Offset,T};
getOffset(<<$\t, T/binary>> ,Offset)	-> {Offset,T};
getOffset(<<Char,T/binary>> ,Offset)	-> getOffset(T,<<Offset/binary,<<Char>>/binary>>).


isValidOffset(<<>> )	                ->	false;
isValidOffset(<<$> ,T/binary >> )	->	isValidOffset(T);
isValidOffset(<<$0,$x,T/binary >> )	->	isHex(T);
isValidOffset(T)			-> 	isDec(T).





getType(<<$\s,T/binary>>,<<>>)	->	getType(T,<<>>);	
getType(<<$\t,T/binary>>,<<>>)	->	getType(T,<<>>);	
getType(<<>> ,Type)			-> {Type,<<>>};
getType(<<$\n, T/binary>> ,Type)	-> {Type,T};
getType(<<$\s, T/binary>> ,Type)	-> {Type,T};
getType(<<$\t, T/binary>> ,Type)	-> {Type,T};
getType(<<Char,T/binary>>,Type)		-> getType(T,<<Type/binary,Char>>).



isValidType(<<>>,<<>>)				->	false;
isValidType(<<>>,Type)				->	isValidType(Type);
isValidType(<<$&,$0,$x,T/binary>>,Type)		->	isHex(T) 	and     isValidType(Type);
isValidType(<<$&,T/binary>>,Type)		->	isDec(T)        and     isValidType(Type);
isValidType(<<$/,T/binary>>,<<"string">>)	->	isValidstr(T); 
isValidType(<<Char,T/binary>>,Type)		->	isValidType(T, <<Type/binary,Char>>).	

isValidType(Type)	when	Type == <<"short">>     ; Type == <<"beshort">> ;  Type == <<"leshort">> ;
				Type == <<"long">> 	; Type == <<"belong">>  ;  Type == <<"lelong">>  ;
				Type == <<"byte">>	; Type == <<"string">>	 		          ->	true;
isValidType(_)		->	false.

%tests for "/[Bbc]*" part of string/[Bbc]* 
isValidstr(<<$b>>)	->	true;
isValidstr(<<$B>>)	->	true;
isValidstr(<<$c>>)	->	true;
isValidstr(<<Char, T/binary>>)		when Char==$b ;  Char==$B ;	Char==$c 	->	isValidstr(T);
isValidstr(_)	        ->	false.					


getData(<<$\s,T/binary>>,<<>>)	->	getData(T,<<>>);	
getData(<<$\t,T/binary>>,<<>>)	->	getData(T,<<>>);	
getData(<<>> ,Data)					-> {Data,<<>>};
getData(<<$\n, T/binary>> ,Data)	-> {Data,T};
getData(<<$\s, T/binary>> ,Data)	-> {Data,T};
getData(<<$\t, T/binary>> ,Data)	-> {Data,T};
getData(<<$\\,$\s,T/binary>>,Data)	->		getData(T,<<Data/binary,<<$\s>>/binary>>);
getData(<<Char,T/binary>>,Data)		->		getData(T,<<Data/binary,<<Char>>/binary>>).



isValidData(Type,Data)	-> isValidData(Type,Data,isValidType(Type,<<>>)).
isValidData(_,_,false)				->	false;
isValidData(<<"string",_/binary >>,_,true)	->	true;
isValidData(_,<<$0,$x,T/binary>>,true)		-> 	isHex(T);
isValidData(_,Data,true)			->  isDec(Data).




getResult(<<$\s , T/binary>>)	->	getResult(T);
getResult(<<$\t , T/binary>>)	->	getResult(T);
getResult(T)			->	T.



%Edits parsed line, it converts Offset,Type to integer and it fixes Data if it has speacial chars in it.(e.g.\021 octal ,\xa3 hex, \t,\a..)
edit({Offset,Type,Data,Result})	->	{ editOffset(Offset), editType(Type), editData(Type,Data), editResult(Result)}.	

editOffset(<<"0">>)		->	0;
editOffset(<<"0x" ,T/binary>>)	->	hex2int(T);
editOffset(<<"0"  ,T/binary>>)	->	oct2int(T);
editOffset(T)			->	dec2int(T).


editResult(<<"\\b",T/binary>>)	->	T;
editResult(Result)		->	Result.


-define(STRING,<<"string",_/binary>>).

editData(?STRING, <<>>)	  			->	<<>> ;
editData(?STRING, <<"\\a",   T/binary>>)	->	<< 7   , (editData(<<"string">>,T))/binary >> ; %Special chars from ascii e.g. \a,\n...
editData(?STRING, <<"\\b",   T/binary>>)	->	<< $\b , (editData(<<"string">>,T))/binary >> ;
editData(?STRING, <<"\\t",   T/binary>>)	->	<< $\t , (editData(<<"string">>,T))/binary >> ;
editData(?STRING, <<"\\n",   T/binary>>)	->	<< $\n , (editData(<<"string">>,T))/binary >> ;
editData(?STRING, <<"\\v",   T/binary>>)	->	<< $\v , (editData(<<"string">>,T))/binary >> ;
editData(?STRING, <<"\\f",   T/binary>>)	->	<< $\f , (editData(<<"string">>,T))/binary >> ;
editData(?STRING, <<"\\r",   T/binary>>)	->	<< $\r , (editData(<<"string">>,T))/binary >> ;
editData(?STRING, <<"\\x",   T/binary>>)	->	editData1(T) ; %We send Tail another editData which fixes hex strings
editData(?STRING, <<$\\,Char,T/binary>>)	 when Char>=$0 , Char =< $7 ->	editData2((<<Char,T/binary>>)) ; %Fixing octal strings
editData(?STRING, <<$\\,Char,T/binary>>)	->	<< Char, (editData(<<"string">>,T))/binary >> ;
editData(?STRING, <<Char    ,T/binary>>)	->	<< Char, (editData(<<"string">>,T))/binary >> ;
editData(Type	, <<"0x"    ,T/binary>>)	->	<<(hex2int(T)):(mybitsize(Type))>>;
editData(Type	, <<"0"     ,T/binary>>)	->	<<(oct2int(T)):(mybitsize(Type))>>;
editData(Type	, T		       )	->	<<(dec2int(T)):(mybitsize(Type))>>.



%Conversions in hex should be like that,  "\xpk" -> [$p,$k] , "\x10zs" -> [16,$z,$s] ,"\x103" -> [16,$3], "\xrs" -> [$x,$r.$s](since there is no hex digit)
%Below functions have hexedit argument as flag and they converts string including "\x"  into right form.

editData1(<<>>)	    		->  <<$x>>;
editData1(<<Snd>> )		->  editData1({isHexDigit(Snd), Snd });
editData1({true ,Snd})		->  << (hex2int(<<Snd>>)) >>;
editData1({false,Snd})		->  << $x ,Snd>> ;
editData1(<<Fst,Snd,T/binary >>	)		->	editData1(isHexDigit(Fst),isHexDigit(Snd) , <<Fst,Snd>> , T).	

editData1(true, true ,  Digits     ,   T )   -> << << (hex2int(Digits)) >> /binary , (editData(<<"string">>,T))/binary >> ;
editData1(true, false,  <<Fst,Snd>>,   T )   ->	<< << (hex2int(<<Fst>>)) >> /binary ,(editData(<<"string">>,<<Snd,T/binary>>))/binary >> ;
editData1(false ,_   ,  Digits     ,   T )   ->	<< $x ,(editData(<<"string">>,<<Digits/binary,T/binary>>))/binary >> .
 

%Conversions in octal is similar to conversions in hexadecimal e.g. "\10z" -> [8,$z]... 
%After conversion if integer overflows byte, binary type automatically takes mod of number.e.g "\401" yields [1].
editData2(<<Fst>>)	    		->	<<(Fst-$0)>>;
editData2(<<Fst,Snd>>)			->	editData2({isOctDigit(Snd), <<Fst,Snd>> });
editData2({true ,Digits})		->	<< (oct2int(Digits)) >>;
editData2({false,<<Fst,Snd>> })	        ->	<< (Fst-$0),Snd>> ;
editData2(<<Fst,Snd,Thrd,T/binary >>) 	->	editData2(isOctDigit(Snd),isOctDigit(Thrd) , <<Fst,Snd,Thrd>> , T).
						
editData2(true, true,   Digits,  T)	    ->	<< << (oct2int(Digits)) >> /binary , (editData(<<"string">>,T))/binary >> ;
editData2(true, false,  <<Fst,Snd,Thrd>>, T)->	<< << (oct2int(<<Fst,Snd>>)) >> /binary ,(editData(<<"string">>,<< Thrd,T/binary>>))/binary >> ;			
editData2(false ,_,     <<Fst,Snd,Thrd>>,   T)	-> << (Fst-$0) , (editData(<<"string">>,<<Snd,Thrd,T/binary>>))/binary >> .





%Some types takes additional parts like lelong&0xaf3d4523 or string/bC.In EditType functions we seperate them for convenience.
editType( <<"string",$/, T/binary >> )                   ->  {string , T     };
editType( <<"string">>  )                                ->  {string , []    }; 
editType( <<"byte"  >>  )                                ->  {byte   , novalue  	,	little	,	1};
editType( <<"short" >>  )                                ->  {short  , novalue  	,	little	,	2};
editType( <<"leshort">> )                                ->  {leshort, novalue  	,	little	,	2};
editType( <<"beshort">> )                                ->  {beshort, novalue  	,	big		,	2};  
editType( <<"long"  >>  )                                ->  {long   , novalue  	,	little	,	4};
editType( <<"lelong">>  )                                ->  {lelong , novalue  	,	little	,	4};
editType( <<"belong">>  )                                ->  {belong , novalue  	,	big		,	4}; 
editType( <<"byte"    , $& , T/binary>> )                ->  {byte   , hdo2int(T)  	,	little	,	1};
editType( <<"short"   , $& , T/binary>> )                ->  {short  , hdo2int(T)  	,	little	,	2};
editType( <<"leshort" , $& , T/binary>> )                ->  {leshort, hdo2int(T)  	,	little	,	2};
editType( <<"beshort" , $& , T/binary>> )                ->  {beshort, hdo2int(T)  	,	big		,	2};
editType( <<"long"    , $& , T/binary>> )                ->  {long   , hdo2int(T)  	,	little	,	4};
editType( <<"lelong"  , $& , T/binary>> )                ->  {lelong , hdo2int(T)  	,	little	,	4};
editType( <<"belong"  , $& , T/binary>> )                ->  {belong , hdo2int(T)  	,	big		,	4}.




%This function returns the bit size of numeric types.
mybitsize(<<"byte"  	,_/binary>>)	->	8 ;
mybitsize(<<"short"	,_/binary>>)	->	16;
mybitsize(<<"long"  	,_/binary>>)	->	32;
mybitsize(<<"leshort" 	,_/binary>>)	->	16;
mybitsize(<<"lelong"  	,_/binary>>)	->	32;
mybitsize(<<"beshort" 	,_/binary>>)	->	16;
mybitsize(<<"belong"  	,_/binary>>)	->	32 .









%Reads Number of bytes from a binary starting Offset with appropriate Endianness.

readBin(Binary,Offset,Number,Endiannes)  -> readBin(Binary,Offset,Number,Endiannes, Offset + Number =< size(Binary)  ).
												 
												
readBin(Binary,Offset,Number,big    ,true)  -> 
			<<_:Offset/binary , Target:Number/big-unit:8	  , _/binary>> = Binary   , <<Target:Number/unit:8>>;
readBin(Binary,Offset,Number,little ,true)  -> 
			<<_:Offset/binary , Target:Number/little-unit:8   , _/binary>> = Binary   , <<Target:Number/unit:8>>;						               			
readBin(Binary,Offset,Number,native ,true)  -> 
			<<_:Offset/binary , Target:Number/native-unit:8   , _/binary>> = Binary   , <<Target:Number/unit:8>>;							 
readBin(_,_,_,_,false)  -> "error: Binary does not have enough length to read from Offset.".
			


compare(FileData,[])		->  "Unknown Type";
compare(FileData,[H|T])		->  compare(FileData , T , test(FileData,H)).

compare(_,_,{ok,Result})	->  Result;
compare(FileData,T, error)	->  compare(FileData,T).	


%Tests for equality between MagicData and Target which got from FileData.
test(FileData, {Offset, Type , Data, Result})	-> 
						result( Data ==  getTarget( Offset, Type, Data, FileData) ,  Result) . 

result(true , Result)	->  {ok,Result};
result(false, _     )   ->  error.





%This function gets exact Data from FileData placed in offset with appropriate length.
getTarget( Offset, {string 	 , []    }, Data, FileData) 	->	readBin(FileData,Offset,size(Data),big);										
getTarget( Offset, {string   , Flag  }, Data, FileData) 	->	1;
getTarget( Offset, {NumericTypes , novalue  , Endianness , Size}, _   ,FileData) -> readBin(FileData,Offset,Size,Endianness);								
getTarget( Offset, {NumericTypes , AndValue , Endianness , Size}, _   ,FileData) -> readBin(FileData,Offset,Size,Endianness). 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%will be editted.







     			
