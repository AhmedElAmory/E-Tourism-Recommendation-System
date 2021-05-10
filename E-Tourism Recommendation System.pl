offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1),bus).
offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31),10, 1), cabin).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding,50).
customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel,100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin,79).



%possibleSubsetDone

possibleSubset(L,R):-
	subseto(L,L1),
	perm(L1,R).
perm([],[]).

perm([H|T],L) :- perm(T,P), inserto(H,P,L).



inserto(X,L,[X|L]).

inserto(X,[H|T],[H|T1]) :- inserto(X,T,T1).

subseto([], []).

subseto([E|Tail], [E|NTail]):-
  subseto(Tail, NTail).
  
subseto([_|Tail], NTail):-
  subseto(Tail, NTail).

%choosePreferencesDone
choosePreferences(Prefs, ChosenPreferences):-
	subseto(Prefs, PrefsSubset),
	getactsubset(PrefsSubset,ChosenPreferences).


getactsubset([],[]).	

getactsubset([H|T],W):-
	H=activity(L),
	subseto(L,X),
	appendo([activity(X)],R,W),
	getactsubset(T,R).
	
getactsubset([H|T],W)	:- 
	H\=activity(_),
	appendo([H],R,W),
	getactsubset(T,R).
	
appendo([],L,L).
appendo([H|T],L,[H|T1]):- appendo(T,L,T1).

%PreferenceSatisfactionDone

preferenceSatisfaction(Offer, Customer, [], 0).

preferenceSatisfaction(Offer, Customer, [H|T], S):-
	H\=means(_),
	H\=accommodation(_),
	H\=activity(_),
	preferenceSatisfaction(Offer, Customer, T, S).

preferenceSatisfaction(Offer, Customer, [means(Mean)|T], S):-
	offerMean(Offer,Mean),
	customerPreferredMean(Customer,Mean,V),
	preferenceSatisfaction(Offer, Customer, T, X),
	S is V + X.
	
	
preferenceSatisfaction(Offer, Customer, [means(Mean)|T], S):-
	\+offerMean(Offer,Mean),
	preferenceSatisfaction(Offer, Customer, T, S).	

preferenceSatisfaction(Offer, Customer, [accommodation(Acco)|T], S):-
	offerAccommodation(Offer,Acco),
	customerPreferredAccommodation(Customer,Acco,V),
	preferenceSatisfaction(Offer, Customer, T, X),
	S is V + X.
	
preferenceSatisfaction(Offer, Customer, [accommodation(Acco)|T], S):-
	\+offerAccommodation(Offer,Acco),
	preferenceSatisfaction(Offer, Customer, T, S).
	
preferenceSatisfaction(offer(Destination,Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests), Customer, [activity([Act|Tact])|T], S):-
	subseto(Activities,[Act]),
	customerPreferredActivity(Customer,Act,V),
	preferenceSatisfaction(offer(Destination,Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests), Customer, [activity(Tact)|T], X),
	S is V + X.
	
	
preferenceSatisfaction(offer(Destination,Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests), Customer, [activity([Act|Tact])|T], S):-
	\+subseto(Activities,[Act]),
	preferenceSatisfaction(offer(Destination,Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests), Customer, [activity(Tact)|T], S).
	
preferenceSatisfaction(Offer, Customer, [activity([])|T], S):-
	preferenceSatisfaction(Offer, Customer, T, S).	
		
%OverLapDone

notOverlapping(period(P1startyear-P1startmonth-P1startday,P1endyear-P1endmonth-P1endday),period(P2startyear-P2startmonth-P2startday,P2endyear-P2endmonth-P2endday)):-
	P1endyear<P2startyear.
	
notOverlapping(period(P1startyear-P1startmonth-P1startday,P1endyear-P1endmonth-P1endday),period(P2startyear-P2startmonth-P2startday,P2endyear-P2endmonth-P2endday)):-
	P1endyear=P2startyear,
	P1endmonth<P2startmonth.
	
notOverlapping(period(P1startyear-P1startmonth-P1startday,P1endyear-P1endmonth-P1endday),period(P2startyear-P2startmonth-P2startday,P2endyear-P2endmonth-P2endday)):-
	P1endyear=P2startyear,	
	P1endmonth=P2startmonth,
	P1endday<P2startday.
	
notOverlapping(period(P1startyear-P1startmonth-P1startday,P1endyear-P1endmonth-P1endday),period(P2startyear-P2startmonth-P2startday,P2endyear-P2endmonth-P2endday)):-
	P2endyear<P1startyear.
	
notOverlapping(period(P1startyear-P1startmonth-P1startday,P1endyear-P1endmonth-P1endday),period(P2startyear-P2startmonth-P2startday,P2endyear-P2endmonth-P2endday)):-
	P2endyear=P1startyear,
	P2endmonth<P1startmonth.
	
notOverlapping(period(P1startyear-P1startmonth-P1startday,P1endyear-P1endmonth-P1endday),period(P2startyear-P2startmonth-P2startday,P2endyear-P2endmonth-P2endday)):-
	P2endyear=P1startyear,	
	P2endmonth=P1startmonth,
	P2endday<P1startday.	


overlapPeriod(period(P1startyear-P1startmonth-P1startday,P1endyear-P1endmonth-P1endday),period(P2startyear-P2startmonth-P2startday,P2endyear-P2endmonth-P2endday)):-
	\+notOverlapping(period(P1startyear-P1startmonth-P1startday,P1endyear-P1endmonth-P1endday),period(P2startyear-P2startmonth-P2startday,P2endyear-P2endmonth-P2endday)).
	
	
%GetOfferDone

helperGetOffer([],_).

helperGetOffer([H|T],offer(Destination,Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests)):-
	H=dest(X),
	X=Destination,
	helperGetOffer(T,offer(Destination,Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests)).

helperGetOffer([H|T],offer(Destination,Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests)):-
	H=budget(X),
	X>=Cost,
	helperGetOffer(T,offer(Destination,Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests)).
helperGetOffer([H|T],offer(Destination,Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests)):-
	H=mean(X),
	offerMean(offer(Destination,Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests),X),
	helperGetOffer(T,offer(Destination,Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests)).

helperGetOffer([H|T],offer(Destination,Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests)):-
	H=accommodation(X),
	offerAccommodation(offer(Destination,Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests),X),
	helperGetOffer(T,offer(Destination,Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests)).

helperGetOffer([H|T],offer(Destination,Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests)):-
	H=activity(X),
	possibleSubset(Activities,X),
	helperGetOffer(T,offer(Destination,Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests)).

helperGetOffer([H|T],offer(Destination,Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests)):-
	H=period(X,Y),
	Period2=period(X,Y),
	overlapPeriod(Period2,Period),
	helperGetOffer(T,offer(Destination,Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests)).


getOffer(ChosenPrefs,Offer):-
	offerMean(X,_),
	helperGetOffer(ChosenPrefs,X),
	Offer =X.

%recommendOfferForCustomerDone
recommendOfferForCustomer(Prefs,ChosenPrefs,O):-
	choosePreferences(Prefs,ChosenPrefs),
	getOffer(ChosenPrefs,O).	
	
%recommmendOfferDone	
recommendOfferhelper(Customer,Preference,Offer,S):-
	recommendOfferForCustomer(Preference, ChosenPreferences, Offer),
	preferenceSatisfaction(Offer, Customer, Preference, S).
	
recommendOfferHelper2([],[],Offer,[]).
	
recommendOfferHelper2([CustH|CustT],[PrefH|PrefT],Offer,[SH|ST]):-
		recommendOfferhelper(CustH,PrefH,Offer,SH),
		recommendOfferHelper2(CustT,PrefT,Offer,ST).

recommendOfferPredicateMaker(Customer,Sat,customSat(Customer,Sat)).

recommendOfferPredicateListMaker([],[],[]).

recommendOfferPredicateListMaker([CustH|CustT],[SatH|SatT],[CustSatH|CustSatT]):-
	recommendOfferPredicateMaker(CustH,SatH,CustSatH),
	recommendOfferPredicateListMaker(CustT,SatT,CustSatT).

sortCustomSatList(List,Sorted):-
	perm(List,Sorted),
	is_sorted(Sorted).

is_sorted([]).
is_sorted([_]).
is_sorted([customSat(_,X),customSat(_,Y)|T]):- 
	X>=Y,
	is_sorted([customSat(_,Y)|T]).
	
getCustomersList([customSat(Y,_)|T],N,N,[]).

getCustomersList([],N,C,[]):-
C<N.
		
getCustomersList([customSat(Y,_)|T],N,C,[Y|T2]):-
	C<N,
	C1 is C+1,
	getCustomersList(T,N,C1,T2).
			
getOfferGuests(offer(_,_,_,_,_,_,_,N),N).	

recommendOffer(Customers, PreferenceList, Offer, CustomersChosen) :-
	recommendOfferHelper2(Customers,PreferenceList,Offer,SatisfactionList),
	recommendOfferPredicateListMaker(Customers,SatisfactionList,CustomSatList),
	sortCustomSatList(CustomSatList,SortedList),
	getOfferGuests(Offer,N),
	getCustomersList(SortedList,N,0,CustomersChosen).
	
	
%Online_Evaluation_Predicate
	
getAllActivities(L):-
	setof(X,Y^Z^customerPreferredActivity(Y,X,Z),L).
	
mostPreferredActivity(C,A):-
	getAllActivities(L),
	gethighestsat(C,L,S),
	customerPreferredActivity(C,A,S).
	
gethighestsat(C,[],S).	
	
gethighestsat(C,[H|T],S):-
	customerPreferredActivity(C,H,S1),
	S1>S,
	helper(C,[T],S1).
	
gethighestsat(C,[H|T],S):-
	customerPreferredActivity(C,H,S1),
	S1=<S,
	helper(C,[T],S1).	
	

















	

	
	
	
	
	
	
	
	