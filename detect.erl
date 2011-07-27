%---------------------------------------------------------------------
% FILE:              detect.erl
% DESCRIPTION:       Mime Type Detection
% DATE:              26/07/2011
% LANGUAGE PLATFORM: Erlang 5.7.4
% OS PLATFORM:       Ubuntu 2.6.35-28
% AUTHOR:            Mustafa Yavuz  &  Ersan V. Zorlu
% EMAIL:             89.Yavuz@gmail.com  & ersanvuralzorlu@gmail.com
%---------------------------------------------------------------------
-module(detect).
-compile(export_all).
-define(MAX_BYTES_TO_READ,1024*100).



%---------------------------------------------------------------------------------------------------------------
%That is main function takes a filename as argument and starts to test for its mime type.
%---------------------------------------------------------------------------------------------------------------
getType(FileName) 		->	detect ( file:open(FileName,[read,binary]), file:open("magic.mime",[read,binary])  ).
							
detect({error,_},_)				->	"File does not exist.";
detect(_,{error,_})				->	"MagicFile does not exist.";
detect({ok,IoFile},{ok,IoMagic})		->	case  file:read(IoFile , ?MAX_BYTES_TO_READ) of
								eof  		->   file:close(IoFile) , "Empty File";
								{ok, FileData}	->   file:close(IoFile),
							  			     MagicList = parse_all_lines(IoMagic, []),
							                             print(compare(FileData,MagicList))
							end. 
							



%---------------------------------------------------------------------------------------------------------------
%Read lines from a file and store valid magiclines in variable 'Line' with their parsed forms.
%---------------------------------------------------------------------------------------------------------------							
parse_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> parse_all_lines(Device, Accum ++ parse(Line))
    end.





%---------------------------------------------------------------------------------------------------------------
%Prints result.
%---------------------------------------------------------------------------------------------------------------
print(<<$\s,T/binary>>)	->	print(T);
print(Result)		->	binary_to_list(Result).



%---------------------------------------------------------------------------------------------------------------
%Checks whether a given digit is available for Decimal , Hexadecimal and Octal or not.
%---------------------------------------------------------------------------------------------------------------
isDecDigit(Char)	-> 	(Char >= $0) and (Char =< $9).
isOctDigit(Char)	-> 	(Char >= $0) and (Char =< $7).
isHexDigit(Char)	->	(Char >= $0) and (Char =< $9) or   
				(Char >= $a) and (Char =< $f) or 
				(Char >= $A) and (Char =< $F).



%---------------------------------------------------------------------------------------------------------------
%This functions takes binary type as argument and returns if it is Decimal, Hex or Octal number.
%---------------------------------------------------------------------------------------------------------------
isDec(<<>>)	->	false;
isDec(Var)	->	<< <<X>> || <<X>> <= Var , isDecDigit(X)  >> == Var.
isHex(<<>>)	->	false;
isHex(Var)	->	<< <<X>> || <<X>> <= Var , isHexDigit(X)  >> == Var.
isOct(<<>>)	->	false;
isOct(Var)	->	<< <<X>> || <<X>> <= Var , isOctDigit(X)  >> == Var.



%---------------------------------------------------------------------------------------------------------------
%This functions takes binary type as argument and converts it into Decimal from Hexadecimal,Octal,Decimal.
%---------------------------------------------------------------------------------------------------------------
hex2int(Hex) ->
    erlang:list_to_integer((erlang:binary_to_list(Hex)), 16).
    
oct2int(Hex) ->
    erlang:list_to_integer((erlang:binary_to_list(Hex)),  8). 
       
dec2int(Hex) ->
    erlang:list_to_integer((erlang:binary_to_list(Hex)), 10).





%---------------------------------------------------------------------------------------------------------------
%This function converts hex,oct or dec (e.g. 0xaef32 , 01242 , 3563) to decimal.
%---------------------------------------------------------------------------------------------------------------
hdo2int(<<$0>>)	            	-> 0 ;
hdo2int(<<$0 , $x, T/binary>>)	-> hex2int(T);
hdo2int(<<$0 , T/binary>>    )	-> oct2int(T);
hdo2int(T)		        -> dec2int(T).





%---------------------------------------------------------------------------------------------------------------
%This function converts binary to an integer.(e.g. <<35,46,34>> -> 2305570)
%---------------------------------------------------------------------------------------------------------------
bin2int(Binary)	->	BitSize = bit_size(Binary),
			<<Result:BitSize>> = Binary,
			Result.
		




%---------------------------------------------------------------------------------------------------------------
%Here this function parse one Magicline and returns parsed form in a tuple with their validities.
%---------------------------------------------------------------------------------------------------------------
parse(<<$#  ,_/binary>>)	->	[];
parse(<<$\n ,_/binary>>)	->	[];
parse({{_,false},_,_,_})	->	[];
parse({_,{_,false},_,_})	->	[];		
parse({_,_,{_,false},_})	->	[];	
parse({{Offset,true},{Type,true},{Data,true},Result})	->	[edit({Offset,Type,Data,Result})];	
parse(Binary)			->		{Offset,ExOffset} = getOffset(Binary,  <<>>)  ,%essential line parsing start
						{Type  ,ExType}   = getType(ExOffset,  <<>>)  , 
						{Data  ,ExData}   = getData(ExType  ,  <<>>)  ,
						Result    	  = getResult(ExData)         ,
						parse({	{Offset,isValidOffset(Offset)	},
							{Type  ,isValidType(Type,<<>>)	},
							{Data  ,isValidData(Type,Data)	},
						        Result				   }).
								 




%---------------------------------------------------------------------------------------------------------------
%getOffset reads Offset part from a magicline.
%---------------------------------------------------------------------------------------------------------------
getOffset(<<>> ,Offset)			-> {Offset,<<>>};
getOffset(<<$\n, T/binary>> ,Offset)	-> {Offset,T};
getOffset(<<$\s, T/binary>> ,Offset)	-> {Offset,T};
getOffset(<<$\t, T/binary>> ,Offset)	-> {Offset,T};
getOffset(<<Char,T/binary>> ,Offset)	-> getOffset(T,<<Offset/binary,<<Char>>/binary>>).




%---------------------------------------------------------------------------------------------------------------
%Checks for validity of Offset
%---------------------------------------------------------------------------------------------------------------
isValidOffset(<<>> )	                ->	false;
isValidOffset(<<$> ,T/binary >> )	->	isValidOffset(T);
isValidOffset(<<$0,$x,T/binary >> )	->	isHex(T);
isValidOffset(T)			-> 	isDec(T).






%---------------------------------------------------------------------------------------------------------------
%Reads Type part from a magicline.
%---------------------------------------------------------------------------------------------------------------
getType(<<$\s,T/binary>>,<<>>)	->	getType(T,<<>>);	
getType(<<$\t,T/binary>>,<<>>)	->	getType(T,<<>>);	
getType(<<>> ,Type)			-> {Type,<<>>};
getType(<<$\n, T/binary>> ,Type)	-> {Type,T};
getType(<<$\s, T/binary>> ,Type)	-> {Type,T};
getType(<<$\t, T/binary>> ,Type)	-> {Type,T};
getType(<<Char,T/binary>>,Type)		-> getType(T,<<Type/binary,Char>>).





%---------------------------------------------------------------------------------------------------------------
%Checks for validity of Type
%---------------------------------------------------------------------------------------------------------------
isValidType(<<>>,<<>>)				->	false;
isValidType(<<>>,Type)				->	isValidType(Type);
isValidType(<<$&,$0,$x,T/binary>>,Type)		->	isHex(T) 	and     isValidType(Type);
isValidType(<<$&,T/binary>>,Type)		->	isDec(T)        and     isValidType(Type);
isValidType(<<$/>>,<<"string">>)		->	true; 
isValidType(<<$/,T/binary>>,<<"string">>)	->	isValidstr(<<$/,T/binary>>); 
isValidType(<<Char,T/binary>>,Type)		->	isValidType(T, <<Type/binary,Char>>).

	

isValidType(Type)	when	Type == <<"short">>     ; Type == <<"beshort">> ;  Type == <<"leshort">> ;
				Type == <<"long">> 	; Type == <<"belong">>  ;  Type == <<"lelong">>  ;
				Type == <<"byte">>	; Type == <<"string">>	 		          ->	true;
isValidType(_)		->	false.




%---------------------------------------------------------------------------------------------------------------
%Tests for "/[Bbc]*" part of string/[Bbc]*  whether it is in the right form.
%---------------------------------------------------------------------------------------------------------------
isValidstr(<<$b>>)	->	true;
isValidstr(<<$B>>)	->	true;
isValidstr(<<$c>>)	->	true;
isValidstr(<<$/>>)	->	false;
isValidstr(<<"//" , _/binary>>)	->	false;
isValidstr(<<Char, T/binary>>)	when Char==$b ;  Char==$B ;  Char==$c ; Char==$/	->	isValidstr(T);
isValidstr(_)	-> false.




%---------------------------------------------------------------------------------------------------------------
%Reads MagicData from a magicline.
%---------------------------------------------------------------------------------------------------------------
getData(<<$\s,T/binary>>,<<>>)	->	getData(T,<<>>);	
getData(<<$\t,T/binary>>,<<>>)	->	getData(T,<<>>);	
getData(<<>> ,Data)		->      {Data,<<>>};
getData(<<$\n, T/binary>>   ,Data)	-> {Data,T};
getData(<<$\s, T/binary>>   ,Data)	-> {Data,T};
getData(<<$\t, T/binary>>   ,Data)	-> {Data,T};
getData(<<$\\,$\s,T/binary>>,Data)	-> getData(T,<<Data/binary,<<$\s>>/binary>>);
getData(<<Char,T/binary>>   ,Data)	-> getData(T,<<Data/binary,<<Char>>/binary>>).




%---------------------------------------------------------------------------------------------------------------
%Controls whether Data is in the right form.
%---------------------------------------------------------------------------------------------------------------
isValidData(_,<<>>)		   -> false;
isValidData(Type,<<Op,T/binary>>)   when Op == $= ; Op == $! ; Op == $> ;
					 Op == $< ; Op == $& ; Op == $^        -> isValidData(Type,T,isValidType(Type,<<>>));
isValidData(Type,Data)    	   -> isValidData(Type,Data,isValidType(Type,<<>>)).


isValidData(_,_,false)				-> false;
isValidData(<<"string",_/binary >>,_,true)	-> true;
isValidData(_,<<$0,$x,T/binary>>,true)		-> isHex(T);
isValidData(_,Data,true)			-> isDec(Data).




%---------------------------------------------------------------------------------------------------------------
%Gets result part of Magicline.
%---------------------------------------------------------------------------------------------------------------
getResult(<<$\s , T/binary>>)	-> getResult(T);
getResult(<<$\t , T/binary>>)	-> getResult(T);
getResult(T)			-> T.






%---------------------------------------------------------------------------------------------------------------
%Edits parsed line, it converts Offset,Type to integer and it fixes Data if it has speacial chars in it.(e.g.\021 octal ,\xa3 hex, \t,\a..)
%---------------------------------------------------------------------------------------------------------------
edit({Offset,Type,Data,Result})	->	{ editOffset(Offset), editType(Type), editData(Type,Data), editResult(Result)}.	

editOffset(T)               			-> editOffset(T , 0).
editOffset(<<"0">> , Level)                	-> {0,Level};
editOffset(<<">"  ,T/binary>> , Level)    	-> editOffset(T , Level + 1);
editOffset(<<"0x" ,T/binary>> , Level)    	-> {hex2int(T)  , Level};
editOffset(<<"0"  ,T/binary>> , Level)    	-> {oct2int(T)  , Level};
editOffset(T , Level)                    	-> {dec2int(T)  , Level}.


editResult(<<>>)		-> <<>>;
editResult(<<"\\b",T/binary>>)	-> editResult(T);
editResult(T)			->  Size=size(T)-1, <<Result:Size/binary , Ch >> = T , editResult(Result, Ch).
editResult(Result, $\n) -> Result;
editResult(Result, Ch)  -> <<Result/binary , Ch>>.
				   






%---------------------------------------------------------------------------------------------------------------
%First we obtain head operator of MagicData then we edit main 
%part of MagicData in editData1 function .e.g. for &0xae45df34 this operator is &.
%---------------------------------------------------------------------------------------------------------------
editData(Type,<<$=,Data/binary>> )	->	{$= , editData0(Type,Data)};
editData(Type,<<$!,Data/binary>> )	->	{$! , editData0(Type,Data)};
editData(Type,<<$>,Data/binary>> )	->	{$> , editData0(Type,Data)};
editData(Type,<<$<,Data/binary>> )	->	{$< , editData0(Type,Data)};
editData(Type,<<$&,Data/binary>> )	->	{$& , editData0(Type,Data)};
editData(Type,<<$^,Data/binary>> )	->	{$^ , editData0(Type,Data)};
editData(Type,Data 	         )	->	{$= , editData0(Type,Data)}.

-define(STRING,<<"string",_/binary>>).
editData0(?STRING, <<>>)	  		->	<<>> ;
editData0(?STRING, <<"\\a",   T/binary>>)	->	<< 7   , (editData0(<<"string">>,T))/binary >> ; %Special chars from ascii e.g. \a,\n...
editData0(?STRING, <<"\\b",   T/binary>>)	->	<< $\b , (editData0(<<"string">>,T))/binary >> ;
editData0(?STRING, <<"\\t",   T/binary>>)	->	<< $\t , (editData0(<<"string">>,T))/binary >> ;
editData0(?STRING, <<"\\n",   T/binary>>)	->	<< $\n , (editData0(<<"string">>,T))/binary >> ;
editData0(?STRING, <<"\\v",   T/binary>>)	->	<< $\v , (editData0(<<"string">>,T))/binary >> ;
editData0(?STRING, <<"\\f",   T/binary>>)	->	<< $\f , (editData0(<<"string">>,T))/binary >> ;
editData0(?STRING, <<"\\r",   T/binary>>)	->	<< $\r , (editData0(<<"string">>,T))/binary >> ;
editData0(?STRING, <<"\\x",   T/binary>>)	->	editData1(T) ; %We send Tail another editData which fixes hex strings
editData0(?STRING, <<$\\,Char,T/binary>>)	 when Char>=$0 , Char =< $7 ->	editData2((<<Char,T/binary>>)) ; %Fixing octal strings
editData0(?STRING, <<$\\,Char,T/binary>>)	->	<< Char, (editData0(<<"string">>,T))/binary >> ;
editData0(?STRING, <<Char    ,T/binary>>)	->	<< Char, (editData0(<<"string">>,T))/binary >> ;
editData0(Type	 , <<"0x"    ,T/binary>>)	->	<<(hdo2int(<<"0x" ,T/binary>>)):(mybitsize(Type))>>;
editData0(Type	 , <<"0"     ,T/binary>>)	->	<<(hdo2int(<<"0"  ,T/binary>>)):(mybitsize(Type))>>;
editData0(Type	 , T		        )	->	<<(dec2int(T)):(mybitsize(Type))>>.







%---------------------------------------------------------------------------------------------------------------
%Conversions in hex should be like that,  "\xpk" -> [$p,$k] , "\x10zs" -> [16,$z,$s] ,"\x103" -> [16,$3], 
%"\xrs" -> [$x,$r.$s](since there is no hex digit)
%---------------------------------------------------------------------------------------------------------------
editData1(<<>>)	    			->  <<$x>>;
editData1(<<Snd>> )			->  editData1({isHexDigit(Snd), Snd });
editData1({true ,Snd})		  	->  << (hex2int(<<Snd>>)) >>;
editData1({false,Snd})		  	->  << $x ,Snd>> ;
editData1(<<Fst,Snd,T/binary >>	) 	->  editData1(isHexDigit(Fst),isHexDigit(Snd) , <<Fst,Snd>> , T).	

editData1(true, true ,  Digits     ,   T )   -> << << (hex2int(Digits)) >> /binary , (editData0(<<"string">>,T))/binary >> ;
editData1(true, false,  <<Fst,Snd>>,   T )   ->	<< << (hex2int(<<Fst>>)) >> /binary ,(editData0(<<"string">>,<<Snd,T/binary>>))/binary >> ;
editData1(false ,_   ,  Digits     ,   T )   ->	<< $x ,(editData0(<<"string">>,<<Digits/binary,T/binary>>))/binary >> .






 
%---------------------------------------------------------------------------------------------------------------
%Conversions in octal is similar to conversions in hexadecimal e.g. "\10z" -> [8,$z]... 
%After conversion if integer overflows byte, binary type automatically takes mod of number.e.g "\401" yields [1].
%---------------------------------------------------------------------------------------------------------------
editData2(<<Fst>>)	    		->	<<(Fst-$0)>>;
editData2(<<Fst,Snd>>)			->	editData2({isOctDigit(Snd), <<Fst,Snd>> });
editData2({true ,Digits})		->	<< (oct2int(Digits)) >>;
editData2({false,<<Fst,Snd>> })	        ->	<< (Fst-$0),Snd>> ;
editData2(<<Fst,Snd,Thrd,T/binary >>) 	->	editData2(isOctDigit(Snd),isOctDigit(Thrd) , <<Fst,Snd,Thrd>> , T).
						
editData2(true, true,   Digits	       	,  T)	-> << << (oct2int(Digits)) >> /binary , (editData0(<<"string">>,T))/binary >> ;
editData2(true, false,  <<Fst,Snd,Thrd>>,  T) 	-> << << (oct2int(<<Fst,Snd>>)) >> /binary ,(editData0(<<"string">>,<< Thrd,T/binary>>))/binary >> ;
editData2(false ,_,     <<Fst,Snd,Thrd>>,  T)	-> << (Fst-$0) , (editData0(<<"string">>,<<Snd,Thrd,T/binary>>))/binary >> .




%---------------------------------------------------------------------------------------------------------------
%Some types takes additional parts like lelong&0xaf3d4523 or string/bC.In EditType functions we seperate them for convenience.
%---------------------------------------------------------------------------------------------------------------
editType( <<"string",$/, T/binary >> )                   ->  {string , binary_to_list(T)};
editType( <<"string">>  )                                ->  {string , []    		}; 
editType( <<"byte"  >>  )                                ->  {byte   , novalue  	,	little	,	1};
editType( <<"short" >>  )                                ->  {short  , novalue  	,	little	,	2};
editType( <<"leshort">> )                                ->  {leshort, novalue  	,	little	,	2};
editType( <<"beshort">> )                                ->  {beshort, novalue  	,	big	,	2};  
editType( <<"long"  >>  )                                ->  {long   , novalue  	,	little	,	4};
editType( <<"lelong">>  )                                ->  {lelong , novalue  	,	little	,	4};
editType( <<"belong">>  )                                ->  {belong , novalue  	,	big	,	4}; 
editType( <<"byte"    , $& , T/binary>> )                ->  {byte   , hdo2int(T)  	,	little	,	1};
editType( <<"short"   , $& , T/binary>> )                ->  {short  , hdo2int(T)  	,	little	,	2};
editType( <<"leshort" , $& , T/binary>> )                ->  {leshort, hdo2int(T)  	,	little	,	2};
editType( <<"beshort" , $& , T/binary>> )                ->  {beshort, hdo2int(T)  	,	big	,	2};
editType( <<"long"    , $& , T/binary>> )                ->  {long   , hdo2int(T)  	,	little	,	4};
editType( <<"lelong"  , $& , T/binary>> )                ->  {lelong , hdo2int(T)  	,	little	,	4};
editType( <<"belong"  , $& , T/binary>> )                ->  {belong , hdo2int(T)  	,	big	,	4}.








%---------------------------------------------------------------------------------------------------------------
%This function returns the bit size of numeric types.
%---------------------------------------------------------------------------------------------------------------
mybitsize(<<"byte"  	,_/binary>>)	->	8 ;
mybitsize(<<"short"	,_/binary>>)	->	16;
mybitsize(<<"long"  	,_/binary>>)	->	32;
mybitsize(<<"leshort" 	,_/binary>>)	->	16;
mybitsize(<<"lelong"  	,_/binary>>)	->	32;
mybitsize(<<"beshort" 	,_/binary>>)	->	16;
mybitsize(<<"belong"  	,_/binary>>)	->	32 .







%---------------------------------------------------------------------------------------------------------------
%Compare function test file for all Magiclines entirely.When it succeeds the test ends.
%---------------------------------------------------------------------------------------------------------------
-define(MYPARSE , [{{Offset , Level} , Type , Data , R } | Tail ] ).

compare(_,[])                                   ->  <<"Unknown Type">>;
compare(FileData,?MYPARSE)   when Level == 0    ->  compare(FileData , Tail , test(FileData,hd(?MYPARSE)));
compare(FileData,[_|T])                         ->  compare(FileData , T ).

compare(FileData, Data , {ok , Result})         ->  compare1(FileData , Data , true, 0 , Result);
compare(FileData, Data , error        )         ->  compare(FileData,Data).    




compare1(_ , [] ,  _  , _ , Result)                            -> Result ;
compare1(FileData , ?MYPARSE , _, _ , <<>>) when Level == 0    -> compare(FileData , ?MYPARSE);        
compare1(_ , [{{_ , 0}, _ , _ , _}|_] ,  _  , _ , Result )     -> Result ;

compare1( FileData , ?MYPARSE , false  , OldLevel , Result)   when Level < OldLevel + 1   ->
                            compare2( FileData , Tail , Level, Result , test(FileData , hd(?MYPARSE)) );

compare1( FileData , ?MYPARSE , true   , OldLevel , Result)   when Level < OldLevel + 2   ->
                            compare2( FileData , Tail , Level  , Result , test(FileData  , hd(?MYPARSE)));
                            
compare1( FileData , [_|T]    , Bool       , OldLevel  , Result )   	->  compare1(FileData , T , Bool , OldLevel , Result).
compare2( FileData , Data , OldLevel , Result , {ok , Res} )   		->  compare1(FileData , Data , true , OldLevel , <<Result/binary ,$\s, Res/binary>>);
compare2( FileData , Data , OldLevel , Result , error      )   		->  compare1(FileData , Data , false , OldLevel , Result).









%---------------------------------------------------------------------------------------------------------------
%Tests for equality between MagicData and Target which got from FileData.
%---------------------------------------------------------------------------------------------------------------
test(FileData, {Offset, Type , {Op,Data}, Result})	->  Target = getTarget( Offset, Type, Data, FileData),
				                            test(Target,{Op,Data},Type,Result).
													
test({ok,Target} , {$=, Data} , _    , Result)	->	result(Data == Target , Result);
test({ok,Target} , {$!, Data} , _    , Result)	->	result(Data /= Target , Result);
test({ok,Target} , {$>, Data} , _    , Result)	->	result(Data <  Target , Result);
test({ok,Target} , {$<, Data} , _    , Result)	->	result(Data >  Target , Result);
test({ok,Target} , {$&, Data} , Type , Result)	->	result( and_xor_test(Type,Data,Target,$&) ,  Result); 
test({ok,Target} , {$^, Data} , Type , Result)	->	result( and_xor_test(Type,Data,Target,$^) ,  Result); 
test({error,_  } , _          , _    , _     )	->   	error.


and_xor_test({_,_,_,TypeSize},Data,Target,Operator)	->	NewTarget = bin2int(Target),
								NewData   = bin2int(Data),
								TypebitSize=TypeSize*8,
								Result = bin2int (<<(NewTarget band NewData):TypebitSize>>), 
								and_xor_test( Result == NewData  , Operator ).

and_xor_test(true   ,$&)	-> true;
and_xor_test(false  ,$^)	-> true;
and_xor_test(_      ,_ )	-> false.

												
result(true , Result)	->  {ok,Result};
result(false, _     )   ->  error.








%---------------------------------------------------------------------------------------------------------------
%This function gets exact Data from FileData placed in offset with appropriate length.
%---------------------------------------------------------------------------------------------------------------
getTarget( Offset, {string   , Flags  }               , Data, FileData)		-> readString(FileData, Offset,Data,Flags);
getTarget( Offset, {_ , AndValue  , Endianness , Size}, _   , FileData) 	-> Target = readBin(FileData,Offset,Size,Endianness),
										   readNumerics(Target , AndValue , Size).
																		   
readNumerics({ok,Target}    , novalue  , _    ) 	->	{ok,Target};
readNumerics({ok,Target}    , AndValue , Size ) 	->	{ok,<<(AndValue  band bin2int(Target)):Size/unit:8>>};
readNumerics({error,Reason} , _        , _    ) 	->	{error,Reason}.	









%---------------------------------------------------------------------------------------------------------------		   
%Reads Number of bytes from a binary starting Offset with appropriate Endianness.
%---------------------------------------------------------------------------------------------------------------
readBin(Binary,{Offset , _},Number,Endiannes)  -> readBin(Binary,Offset,Number,Endiannes, Offset + Number =< size(Binary)  ).
												 
												
readBin(Binary,Offset,Number,big    ,true ) -> 
			<<_:Offset/binary , Target:Number/big-unit:8	  , _/binary>> = Binary   , {ok,<<Target:Number/unit:8>>};
readBin(Binary,Offset,Number,little ,true ) -> 
			<<_:Offset/binary , Target:Number/little-unit:8   , _/binary>> = Binary   , {ok,<<Target:Number/unit:8>>};
readBin(Binary,Offset,Number,native ,true ) -> 
			<<_:Offset/binary , Target:Number/native-unit:8   , _/binary>> = Binary   , {ok,<<Target:Number/unit:8>>};
readBin(_     ,_     ,_     ,_      ,false) ->{error, "Binary does not have enough length to be read from Offset."}.
					   
		   
		   






%---------------------------------------------------------------------------------------------------------------		   
%Reads string to be compared from target
%---------------------------------------------------------------------------------------------------------------
readString(FileData, Offset,Data,[])   	   ->	readBin(FileData, Offset ,size(Data),big); %Read string if has no Flag.
readString(FileData, Offset,Data,Flags)    ->	Target = readBin(FileData, Offset ,size(Data)+20,big),
						readString(Data ,Target,lists:member($b, Flags), lists:member($c, Flags), lists:member($B, Flags)).
						
						
readString(Data, {ok,Target}, true  ,false, false)   ->   {ok,editFlag_b(Target, Data)};
readString(Data, {ok,Target}, true  ,false, true )   ->   {ok,editFlag_B(editFlag_b(Target,  Data),   Data)};
readString(Data, {ok,Target}, true  ,true , false)   ->   {ok,editFlag_c(editFlag_b(Target , Data),   Data)};
readString(Data, {ok,Target}, true  ,true , true )   ->   {ok,editFlag_B(editFlag_c(editFlag_b(Target , Data),   Data), Data)};
readString(Data, {ok,Target}, false ,true , false)   ->   {ok,editFlag_c(Target, Data)};
readString(Data, {ok,Target}, false ,false, true )   ->   {ok,editFlag_B(Target, Data)};
readString(Data, {ok,Target}, false ,true , true )   ->   {ok,editFlag_c(editFlag_B(Target , Data),   Data)};
readString(_   , {error,Reason},_    ,_    ,_    )   ->   {error,Reason}.









%---------------------------------------------------------------------------------------------------------------		   
%Edits read string according to rules of Flag 'b'
%---------------------------------------------------------------------------------------------------------------
editFlag_b(_, <<>>)                            	   -> <<>>;
editFlag_b(<<$\s, T/binary>>, <<$\s, T1/binary>>)  -> <<$\s, (editFlag_b(removeBlanks(T), T1))/binary>>;
editFlag_b(Target, <<$\s, T1/binary>>)    	   -> <<$\s, (editFlag_b(Target, T1))/binary>>;
editFlag_b(<<H  , T/binary>>, <<_  , T1/binary>>)  -> <<H  , (editFlag_b(T, T1))/binary>>.





%---------------------------------------------------------------------------------------------------------------		   
%editFlag_c(Target,Data):
%If a char is lowercase in Data, corresponding char in Target turns into lowecase if it is uppercase in Data it remains same in Target. 
%---------------------------------------------------------------------------------------------------------------
editFlag_c(_ , <<>>)                              	-> <<>>;
editFlag_c(<<H , T/binary >> , <<H1, T1/binary>> )  	 when H1  >= $a  , H1 =< $z ,
                					      H   >= $A  , H  =< $Z  ->
                						<<(H bor H1) , (editFlag_c(T,T1))/binary>>;
editFlag_c(<<H , T/binary >> , <<_ , T1/binary>> )  	-> <<H, (editFlag_c(T,T1))/binary>>. 



%---------------------------------------------------------------------------------------------------------------		   
%Edits read string according to rules of Flag 'B' .
%---------------------------------------------------------------------------------------------------------------
editFlag_B(_, <<>>)                            	   ->  <<>>;
editFlag_B(<<>>, _)                            	   ->  <<>>;
editFlag_B(<<$\s, T/binary>>, <<$\s, T1/binary>>)  ->  <<$\s , (editFlag_B(removeBlanks(T), T1))/binary>>;
editFlag_B(<<H  , T/binary>>, <<_  , T1/binary>>)  ->  <<H,    (editFlag_B(T, T1))/binary >>.



%---------------------------------------------------------------------------------------------------------------		   
%Removes initial blanks.
%---------------------------------------------------------------------------------------------------------------
removeBlanks(<<$\s, T/binary>>)	->	removeBlanks(T);
removeBlanks(Target)	        ->	Target.
