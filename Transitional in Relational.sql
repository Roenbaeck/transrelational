--------------------------------------- RELATIONAL MODEL ---------------------------------------
--
--   Script by Lars Rönnbäck
--   Based on the paper "Modeling Conflicting, Unreliable, and Varying Information"
--   https://www.researchgate.net/publication/329352497_Modeling_Conflicting_Unreliable_and_Varying_Information
--
--   This script creates a relationally modeled implementation of Transitional Modeling, with 
--   some limitiations due to missing features in SQL Server.
--
--   The script has only been tested in SQL Server 2019. A Docker image is available that can be run
--   on most operating systems, and I followed this guide for macOS: 
--   https://database.guide/install-sql-server-2019-on-a-mac/
--
--   You can use Azure Data Studio, also for most operating systems, to connect to the Docker image: 
--   https://docs.microsoft.com/en-us/sql/azure-data-studio/download-azure-data-studio
--
--   Version: 20181217.2   Better XML in the views.
--   Version: 20210703.1   Assertions are now meta-posits, reflecting our latest research.
--                         Updated other terminology to also reflect later research.
--                         Appearing values are now XML, to indicate that they can have structure.
--
--
drop view if exists [Check_for_Contradictions];
drop function if exists [Information_in_Effect];

drop view if exists [v_Assertion];
drop view if exists [v_Posit];

drop proc if exists AddPosit;
drop proc if exists AddAppearanceSet;
drop proc if exists AddAppearance;
drop proc if exists AddRole;
drop proc if exists AddThing;

drop type if exists Things;
drop type if exists Certainty;
drop rule if exists Certainty_Interval;
drop default if exists Complete_Uncertainty;
drop table if exists [Posit];
drop table if exists [Appearance_in_AppearanceSet];
drop table if exists [AppearanceSet];
drop table if exists [Appearance];
drop table if exists [Role];
drop table if exists [Internal];
drop table if exists [Thing];

-- A table to store the unique identifiers of things.
-- Where a thing is that which is sufficiently distinguishable 
-- from something else to be told apart.
create table [Thing] (
	[Thing_Identity] bigint identity(1, 1) not null, 
	-- Enforce uniqueness of the unique identifiers.
	-- Note that primary keys enforce uniqueness as well as cluster
	-- the underlying table for better performance, and is needed
	-- in order to reference these using foreign key references.
	constraint [unique_and_referenceable_Thing_Identity] primary key clustered (
		[Thing_Identity]
	)
);

-- We will also have internal things, needed to keep the relational
-- model happy, but that we never expose outside of the database.
-- These will have their own number sequence so there is no risk
-- of collisions with the things above.
create table [Internal] (
	[Internal_Identity] bigint identity(1, 1) not null, 
	constraint [unique_and_referenceable_Internal_Identity] primary key clustered (
		[Internal_Identity]
	)
);

-- Roles indicate in which circumstances things appear.
create table [Role] (
	[Role_Identity] bigint not null,
	[Role] varchar(555) not null,
	constraint [Role_is_Internal] foreign key (
		[Role_Identity]
	) references [Internal]([Internal_Identity]),
	constraint [referenceable_Role_Identity] primary key clustered (
		[Role_Identity]
	),
	constraint [unique_Role] unique nonclustered (
		[Role]
	)
);

/*
----------- Excerpt from: Modeling Conflicting, Unreliable, and Varying Information -----------
def. of universal 
A body of information is said to be universal iff positors agree on all appearances.
-----------------------------------------------------------------------------------------------

In order to disagree you must first agree upon something upon which the difference in 
opinion lies. At the very least, positors must agree on appearances. In other words
if two positors want to have an opinion about something, they must first come to the 
conclusion that this something boils down to the same unique identifier for both of them 
and that they mean the same when they talk about the roles it may appear in.

We will assume that talk about "Archie's beard" by any positor means that it is the 
same Archie and the same property they are talking about.
*/
create table [Appearance] (
	[Appearance_Identity] bigint not null,
	[Thing_Identity] bigint not null,
	[Role_Identity] bigint not null,
	constraint [Appearance_is_Internal] foreign key (
		[Appearance_Identity]
	) references [Internal]([Internal_Identity]),
	constraint [ensure_existing_Thing] foreign key (
		[Thing_Identity]
	) references [Thing]([Thing_Identity]),
	constraint [ensure_existing_Role] foreign key (
		[Role_Identity]
	) references [Role]([Role_Identity]),
	constraint [referenceable_Appearance_Identity] primary key clustered (
		[Appearance_Identity]
	),
	constraint [unique_Appearance] unique nonclustered (
		[Thing_Identity],
		[Role_Identity]
	)
);

create table [AppearanceSet] (
	[AppearanceSet_Identity] bigint not null,
	constraint [AppearanceSet_is_Internal] foreign key (
		[AppearanceSet_Identity]
	) references [Internal]([Internal_Identity]),
	constraint [unique_AppearanceSet] primary key clustered (
		[AppearanceSet_Identity]
	)
);	

create table [Appearance_in_AppearanceSet] (
	[AppearanceSet_Identity] bigint not null,
	[Appearance_Identity] bigint not null,
	constraint [reference_to_AppearanceSet] foreign key (
		[AppearanceSet_Identity]
	) references [AppearanceSet]([AppearanceSet_Identity]),
	constraint [reference_to_Appearance] foreign key (
		[Appearance_Identity]
	) references [Appearance]([Appearance_Identity]),
	constraint [unique_Appearance_in_AppearanceSet] primary key clustered (
		[AppearanceSet_Identity],
		[Appearance_Identity]
	)
);	



/*
----------- Excerpt from: Modeling Conflicting, Unreliable, and Varying Information -----------
def. of canonical
A body of information is said to be canonical iff all assertions
are made against posits without negated values.
-----------------------------------------------------------------------------------------------

In practice, the value of a posit may be of any data type, primitive or complex, but due to 
SQL Server lacking support for generics (looks like <T> in many programming languages) we 
will limit the column for the "appearing value" to xml. The "appearance time" may also be any 
time type, exact or fuzzy, in order to represent that the value appeared exactly since some 
specified time or inexactly within some period of time. Here the column is limited 
to a datetime.

Because SQL Server cannot enforce uniquness of an xml value, an additional computed column
is added with a hash value of the xml. This computer column can instead be used in the index
to enqure uniqueness of the posit.

Finally, in order to represent information, canonical form is used, which simply means that
values are stored without negation, for example "red" is acceptable, and "not red" is not.
Opposite opinions are instead handled using negative Reliability in assertions.
*/ 
create table [Posit] (
	[Posit_Identity] bigint not null,
	[AppearanceSet_Identity] bigint not null,
	[AppearingValue] xml null, 
    [AppearingValue_Hash] as cast(
        HASHBYTES('SHA2_256', cast([AppearingValue] as varchar(max))) 
        as varbinary(32)
    ) persisted,
	[AppearanceTime] datetime null,
	constraint [ensure_existing_AppearanceSet] foreign key (
		[AppearanceSet_Identity]
	) references [AppearanceSet]([AppearanceSet_Identity]),
	constraint [Posit_is_Thing] foreign key (
		[Posit_Identity]
	) references [Thing]([Thing_Identity]),
	constraint [referenceable_Posit_Identity] primary key clustered (
		[Posit_Identity]
	),
    constraint [unique_Posit] unique nonclustered (
        [AppearanceSet_Identity],
        [AppearingValue_Hash],
        [AppearanceTime]
    )
);

/*
Certainty will be used in the assertion meta-posit to indicate the confidence with which
the asserter ascertains a posit.
*/
go
create rule Certainty_Interval as @certainty between -1 and 1;
go
create default Complete_Uncertainty as 0.0;
go
create type Certainty from decimal(3, 2) not null;
exec sys.sp_bindefault @defname=N'Complete_Uncertainty', @objname=N'Certainty' , @futureonly='futureonly';
exec sys.sp_bindrule @rulename=N'Certainty_Interval', @objname=N'Certainty' , @futureonly='futureonly';
go

/*
Create a stored procedure that adds a thing if it does not already exist
or generates a new one when the input parameter is null
*/
create or alter proc AddThing(@Thing_Identity bigint = null)
as
begin
    set nocount on;
    if @Thing_Identity is null -- we want to generate a new id
    begin
        insert into [Thing] default values;
        set @Thing_Identity = SCOPE_IDENTITY();
    end
    else -- an identity was passed and should be added if it doesn't already exist
    begin
        declare @Existing_Thing_Identity bigint;
        set @Existing_Thing_Identity = (
            select [Thing_Identity] from [Thing] where [Thing_Identity] = @Thing_Identity
        );
        if @Existing_Thing_Identity is null 
        begin
            set IDENTITY_INSERT [Thing] on;
            insert into [Thing](Thing_Identity) values (@Thing_Identity);
            set IDENTITY_INSERT [Thing] off;
        end
    end
    return @Thing_Identity;
end
go

/*
Create a stored procedure that adds an internal thing if it does not already exist
or generates a new one when the input parameter is null
*/
create or alter proc AddInternal(@Internal_Identity bigint = null)
as
begin
    set nocount on;
    if @Internal_Identity is null -- we want to generate a new id
    begin
        insert into [Internal] default values;
        set @Internal_Identity = SCOPE_IDENTITY();
    end
    else -- an identity was passed and should be added if it doesn't already exist
    begin
        declare @Existing_Internal_Identity bigint;
        set @Existing_Internal_Identity = (
            select [Internal_Identity] from [Internal] where [Internal_Identity] = @Internal_Identity
        );
        if @Existing_Internal_Identity is null 
        begin
            set IDENTITY_INSERT [Internal] on;
            insert into [Internal](Internal_Identity) values (@Internal_Identity);
            set IDENTITY_INSERT [Internal] off;
        end
    end
    return @Internal_Identity;
end
go

/*
Create a stored procedure that adds a role if it does not already exist. 
Roles have internal identities. 
*/
create or alter proc AddRole(@role varchar(555))
as
begin
    set nocount on;
    declare @Role_Identity bigint = (
        select [Role_Identity] from [Role] where [Role] = @role
    );
    if @Role_Identity is null
    begin
        exec @Role_Identity = AddInternal;

        insert into [Role] ([Role_Identity], [Role])
        values (@Role_Identity, @role);
    end
    return @Role_Identity;
end
go

/*
----------- Excerpt from: Modeling Conflicting, Unreliable, and Varying Information -----------
def. of a body of information
A body of information is a set of true assertions.

def. of exclusive
A body of information is said to be exclusive iff no assertions
in which only the reliability differs exist.
-----------------------------------------------------------------------------------------------

Assertions will be modeled as meta-posits, posits that talk about other posits. In order 
to do so we will need to introduce two roles, reserved for this purpose.
*/

-- delete Role; delete Thing; -- (only if we need to restart from here)
exec AddRole 'posit';
exec AddRole 'ascertains';

-- there should now be two roles in the table
select * from [Role];

/*
This view "assembles" the posit from it's constituting parts, which has been spread out
into different tables in the relational representation.
*/
go
create or alter view [v_Posit] 
as
select
	p.[Posit_Identity],
	p.[AppearanceSet_Identity],
	p.[AppearingValue],
	p.[AppearingValue_Hash],
	p.[AppearanceTime],
	(
		select
			p.[Posit_Identity] as [@Thing],
			(
				select
					a.[Thing_Identity] as [@Thing],
					r.[Role] as [@Role]
				from
					[AppearanceSet] s
				join
					[Appearance_in_AppearanceSet] i
				on
					i.[AppearanceSet_Identity] = s.[AppearanceSet_Identity]
				join
					[Appearance] a
				on
					a.[Appearance_Identity] = i.[Appearance_Identity]
				join
					[Role] r
				on
					r.[Role_Identity] = a.[Role_Identity]
				where
					s.[AppearanceSet_Identity] = p.[AppearanceSet_Identity]
				order by
					r.[Role] asc
				for xml path('Appearance'), type
			) as [AppearanceSet],
			p.[AppearingValue],
			p.[AppearanceTime] 
		for xml path('Posit'), type
	) as [PositXML]
from
	[Posit] p;
go

/*
This view "assembles" the assertion from it's constituting parts, which has been spread out
into different tables in the relational representation, as well as being meta-posits on
the form: 
[{(Posit_Identity, 'posit'), (Thing_Identity, 'ascertains')}, <Certainty>, <AppearanceTime>]
*/

create or alter view [v_Assertion]
as 
with assertion as (
    select
        PositXML.value(
            '(//Appearance[@Role="ascertains"])[1]/@Thing', 
            'bigint'
        ) as [Asserter_Identity],
        PositXML.value(
            '(//Appearance[@Role="posit"])[1]/@Thing', 
            'bigint'
        ) as [Posit_Identity],
        [Posit_Identity] as [Assertion_Identity], 
        [AppearanceSet_Identity],
        [AppearingValue],
        [AppearingValue_Hash],
        [AppearanceTime], 
        [PositXML]
    from 
        [v_Posit]
    where 
        PositXML.exist('/Posit/AppearanceSet/Appearance[@Role="ascertains"]') = 1
)
select
	a.[Assertion_Identity],
	a.[Asserter_Identity],
    a.[Posit_Identity],
	p.[AppearanceSet_Identity],
	p.[AppearingValue],
	p.[AppearingValue_Hash],
	p.[AppearanceTime],
    a.[AppearingValue].value('.', 'Certainty') as Certainty,
	a.[AppearanceTime] as [AssertionTime],
	(
		select
			a.[Posit_Identity] as [@Thing],
			a.[Asserter_Identity] as [Asserter/@Thing],
			p.[PositXML].query('.'),
			a.[AppearingValue].query('.') as [Certainty],
			a.[AppearanceTime] as [AssertionTime]
		for xml path('Assertion'), type
	) as [AssertionXML]
from 
    assertion a
join
	[v_Posit] p
on
	p.[Posit_Identity] = a.[Posit_Identity]
go

/*
Adds an appearance provided that it does not already exist
*/
create or alter proc AddAppearance(@Thing_Identity bigint, @Role_Identity bigint)
as
begin
    set nocount on;
    declare @Appearance_Identity bigint;
    set @Appearance_Identity = (
        select [Appearance_Identity] from [Appearance]
        where [Thing_Identity] = @Thing_Identity and [Role_Identity] = @Role_Identity
    );
    if @Appearance_Identity is null
    begin
        exec @Appearance_Identity = AddInternal;
        insert into [Appearance] ([Appearance_Identity], [Thing_Identity], [Role_Identity])
        values (@Appearance_Identity, @Thing_Identity, @Role_Identity);
    end
    return @Appearance_Identity;
end
go

/* 
Create a table type that hold identities of things.
*/
create type Things 
as table (
    [Thing_Identity] bigint not null primary key
);
go

/*
Create a procedure that adds an appearance set if is doesn't exist.
*/
create or alter proc AddAppearanceSet(@Appearances Things READONLY)
as
begin
    declare @AppearanceSet_Identity bigint;
    set @AppearanceSet_Identity = (
        select [AppearanceSet_Identity] 
        from [Appearance_in_AppearanceSet] s
        join @Appearances a
        on a.[Thing_Identity] = s.[Appearance_Identity]
        group by [AppearanceSet_Identity] 
        having count(*) = (select count(*) from @Appearances)
        except 
        select distinct [AppearanceSet_Identity] 
        from [Appearance_in_AppearanceSet] 
        where [Appearance_Identity] not in (select * from @Appearances)
    )
    if @AppearanceSet_Identity is null 
    begin
        exec @AppearanceSet_Identity = AddInternal;
        insert into [AppearanceSet]([AppearanceSet_Identity])
        values (@AppearanceSet_Identity);
        insert into [Appearance_in_AppearanceSet]([AppearanceSet_Identity], [Appearance_Identity])
        select @AppearanceSet_Identity, [Thing_Identity] from @Appearances;
    end
    return @AppearanceSet_Identity;
end
go

/*
In order to easily add some example, we will create a stored procedure that parses a posit
input as a string on the exemplified format: 

    555 [{(42, posit), (43, ascertains)}, 1.0, 2021-07-04]

    555         the identity of the posit that follows
    42          the identity of an (in this case) asserted posit
    posit       the role 42 plays
    43          the identity of the (in this case) asserter who is ascertaining the posit
    ascertains  the role 43 plays
    1.0         the (in this case) certainty with which the asserter is ascertaining the posit
    2021-07-04  the point in time that the value is appearing at (in this case assertion time)
    
*/

create or alter proc AddPosit(@posit varchar(max))
as
begin
    set NOCOUNT on;
    declare @Posit_Identity bigint;
    set @Posit_Identity = TRIM(SUBSTRING(
        @posit, 
        1, 
        CHARINDEX('[', @posit) - 1
    ));
    if not exists (select [Posit_Identity] from [Posit] where [Posit_Identity] = @Posit_Identity)
    begin 
        declare @AppearingValue varchar(max);
        set @AppearingValue = TRIM(SUBSTRING(
                @posit, 
                CHARINDEX('"', @posit) + 1, 
                CHARINDEX('"', @posit, CHARINDEX('"', @posit) + 1) - CHARINDEX('"', @posit) - 1
        ));
        declare @AppearanceTime varchar(max);
        set @AppearanceTime = TRIM(SUBSTRING(
                @posit, 
                CHARINDEX('"', @posit, CHARINDEX('"', @posit) + 1) + 2, 
                CHARINDEX(']', @posit) - CHARINDEX('"', @posit, CHARINDEX('"', @posit) + 1) - 2
        ));
        declare @AppearanceSet varchar(max);
        set @AppearanceSet = TRIM(SUBSTRING(
                @posit, 
                CHARINDEX('{', @posit) + 1, 
                CHARINDEX('}', @posit) - CHARINDEX('{', @posit) - 1
        ));
        declare @DynamicQuery varchar(max);
        set @DynamicQuery = 'declare @Appearances Things;' + CHAR(10) + (
            select
                'exec AddThing ' + [Thing_Identity] + ';' + CHAR(10) +  
                'declare @Role_Identity' + cast(ordinal as varchar(10)) + ' bigint;' + CHAR(10) + 
                'exec @Role_Identity' + cast(ordinal as varchar(10)) + ' = AddRole ''' + [Role] + ''';' + CHAR(10) +
                'declare @Appearance_Identity' + cast(ordinal as varchar(10)) + ' bigint;' + CHAR(10) +
                'exec @Appearance_Identity' + cast(ordinal as varchar(10)) + ' = AddAppearance ' + 
                    [Thing_Identity] + ', @Role_Identity' + cast(ordinal as varchar(10)) + ';' + CHAR(10) +
                'insert into @Appearances values (@Appearance_Identity' + cast(ordinal as varchar(10)) + ');' + CHAR(10)
                as [text()]
            from (
                select 
                    max(case when ordinal % 2 = 1 then split end) as [Thing_Identity],
                    max(case when ordinal % 2 = 0 then split end) as [Role], 
                    ROW_NUMBER() over (order by (select 1)) as ordinal
                from (
                    select 
                        TRIM(TRANSLATE(value, '()', '  ')) as split,
                        ROW_NUMBER() over (order by (select 1)) as ordinal
                    from 
                        string_split(@AppearanceSet, ',')
                ) s
                group by (ordinal + 1) / 2
            ) a
            for xml path(''), type
        ).value('.', 'varchar(max)') + 
        'declare @AppearanceSet_Identity bigint;' + CHAR(10) + 
        'exec @AppearanceSet_Identity = AddAppearanceSet @Appearances;' + CHAR(10) + 
        'insert into [Posit]([Posit_Identity], [AppearanceSet_Identity], [AppearingValue], [AppearanceTime])' + CHAR(10) + 
        'values (' + 
        cast(@Posit_Identity as varchar(10)) + 
        ', @AppearanceSet_Identity, ''' + 
        @AppearingValue + ''', ''' + 
        @AppearanceTime + ''')';

        exec AddThing @Posit_Identity;
        --select @AppearanceSetQuery;
        exec (@DynamicQuery);
    end
    return @Posit_Identity;
end

--------------------------------------- EXAMPLES ---------------------------------------
-- Let's create some individuals that the examples can revolve around
exec AddPosit '60000 [{(42, name)}, "Archie", 1972-08-20]';
exec AddPosit '60001 [{(43, name)}, "Bella", 1972-02-13]';
exec AddPosit '60002 [{(44, name)}, "Modeler", 2009-09-21]';
exec AddPosit '60003 [{(45, name)}, "Script", 2021-06-30]';
exec AddPosit '60004 [{(46, name)}, "Disagreer", 1955-05-15]';

-- now let's add some information about Archie's beard
exec AddPosit '60005 [{(42, has beard)}, "fluffy red", 2018-12-01 12:00]';
-- let Bella assert this
exec AddPosit '60006 [{(60005, posit), (43, ascertains)}, "1.0", 2018-12-13 15:30]';

-- there are posits for the names, the beard, and the assertion meta-posit
select * from [v_Posit];
-- now we have one assertion (a posit and a related meta-posit)
-- note that none of the names have been asserted, so they will not show up here
select * from [v_Assertion];

-- let's add a contradictory statement
exec AddPosit '60007 [{(42, has beard)}, "shaved clean", 2018-12-01 12:00]';
-- let the Disagreer assert this, so create another assertion thing
exec AddPosit '60008 [{(60007, posit), (46, ascertains)}, "1.0", 2018-12-13 15:30]';

-- now there are two assertions, representing the conflicting views of Bella and the Disagreer
-- with respect to Archie's beard
select * from [v_Assertion];

-- a few minutes later the Disagreer strengthens their standpoint by stating that they
-- are certain of the opposite of Archie's beard being fluffy red
exec AddPosit '60009 [{(60005, posit), (46, ascertains)}, "-1.0", 2018-12-13 15:35]';

-- let Bella assert that there is a very small chance it actually was shaved clean
exec AddPosit '60010 [{(60007, posit), (43, ascertains)}, "0.05", 2018-12-13 15:35]';

-- with this last assertion, the Disagreer still has a consistent point of view, 
-- but Bella is now contradicting herself
select * from [v_Assertion];

-- let's add a statement that Archie was shaved clean somewhat later 
exec AddPosit '60011 [{(42, has beard)}, "shaved clean", 2018-12-01 13:00]';

-- now there are two posits stating Archie is shaved clean, which if treated
-- as if they are in succession, the second is a restatement of the first
select * from [v_Posit] where PositXML.exist('//Appearance[@Role="has beard"]') = 1;

-- now Bella asserts that at 13:00 Archie had shaved
exec AddPosit '60012 [{(60011, posit), (43, ascertains)}, "1.0", 2018-12-13 15:35]';

-- keeping the restatement is important, since they can be asserted with different certainties
select * from [v_Assertion];


/*
----------- Excerpt from: Modeling Conflicting, Unreliable, and Varying Information -----------
def. of the information in effect
Let A be a body of information. The information in effect is a
subset A(T@, t@) ⊂ A given a bitemporal point in assertion
and appearance time (T@, t@) ∈ T × t. Assuming P, α, T, D, v,
and t are free variables, all assertions !(P, p, α, T) ∈ A(T@, t@)
with p = [D, v, t] are found using the following selection
criteria:
1. Let A1 ⊂ A be the assertions in A satisfying T ≤ T@ and
   t ≤ t@.
2. Let A2 ⊂ A1 be the assertions in A1 with α 6= 0 and the
   latest appearance time t for each combination of P and
   D, excluding those assertions !(P, p, α, T) for which an
   assertion !(P, p, 0, T0) exists with T ≤ T0 ≤ T@.
3. Let A(T@, t@) ⊂ A2 be the assertions from A2 with the
   latest assertion time T for each combination of P, D, and v.
-----------------------------------------------------------------------------------------------

A function for the information in effect is created below according
to the selection criteria defined above.
*/
go
create or alter function [Information_in_Effect] (
	@appearanceTime datetime,
	@assertionTime datetime
)
returns table as return
with A1 as (
	select
		*
	from
		[v_Assertion]
	where
		[AppearanceTime] <= @appearanceTime
	and
		[AssertionTime] <= @assertionTime
),
A2 as (
	select
		a.*
	from
		A1 a
	where
		a.[Certainty] <> 0
	and
		a.[AppearanceTime] = (
			select
				max(s.[AppearanceTime])
			from
				A1 s
			where
				s.[Asserter_Identity] = a.[Asserter_Identity]
			and 
				s.[AppearanceSet_Identity] = a.[AppearanceSet_Identity]
		)
	and not exists (
		select
			x.[Assertion_Identity]
		from
			A1 x
		where
			x.[Asserter_Identity] = a.[Asserter_Identity]
		and 
			x.[Posit_Identity] = a.[Posit_Identity]
		and			
			x.[Certainty] = 0
		and
			x.[AssertionTime] between a.AssertionTime and @assertionTime
	)
), 
A3 as (
	select
		a.*
	from
		A2 a
	where
		a.[AssertionTime] = (
			select
				max(s.[AssertionTime])
			from
				A2 s
			where
				s.[Asserter_Identity] = a.[Asserter_Identity]
			and 
				s.[AppearanceSet_Identity] = a.[AppearanceSet_Identity]
			and	
				s.[AppearingValue_Hash] = a.[AppearingValue_Hash]
		)		
)
select
	*
from 
	A3;
go

-- which information is in effect at 12.00 and 13.00 given what was asserted on or before 15:30?
select * from [Information_in_Effect]('2018-12-01 12:00', '2018-12-13 15:30');
select * from [Information_in_Effect]('2018-12-01 13:00', '2018-12-13 15:30');

-- which information is in effect at 12.00 and 13.00 given what was asserted on or before 15:35?
select * from [Information_in_Effect]('2018-12-01 12:00', '2018-12-13 15:35');
select * from [Information_in_Effect]('2018-12-01 13:00', '2018-12-13 15:35');

/*
----------- Excerpt from: Modeling Conflicting, Unreliable, and Varying Information -----------
def. of symmetric
A body of information is said to be symmetric iff the assertions
!(P, p, −α, T) and !(P, ̄p, α, T) are equivalent for a posit p and its opposite ̄p.

def. of bounded 
A body of information is said to be bounded iff the reliabilities in the assertions 
!(P, p, α, T) and !(P, ̄p, β, T) satisfy ∣α + β∣ = ∣α + β∣^2.

def. of non-contradictory
A body of information is said to be non-contradictory iff for
every information in effect the reliabilities, numbered 1 to n, 
in all positive and negative assertions made by the same positor 
for the same dereferencing set satisfy the inequality:
    n                  n
1/2 ∑ (1 - sign(αi)) + ∑ αi ≤ 1 
   i=1                i=1
-----------------------------------------------------------------------------------------------

The fact that we can talk about contradictions relies on the assumption that our body
of information is symmetric. When a body of information is also bounded it is possible to 
transform all information into canonical form (negated values like "not red" are instead
expressed using "red" and a negative Reliability).
*/

-- we create a view that checks for contradictions according to the inequality above
go
create or alter view [Check_for_Contradictions] 
as
with bitemporalTimepoints as (
	select
		[AppearanceTime], [AssertionTime]
	from 
		(select distinct [AppearanceTime] from v_Assertion) p
	cross join
		(select distinct [AssertionTime] from v_Assertion) a
)
select
	ineq.*,
	case 
		when ConcurringCertainty <= 1 then 'Non-Contradictory'
		else 'Contradictory'
	end as Result
from (
	select
		t.[AppearanceTime],
		t.[AssertionTime],
		iie.[Asserter_Identity],
		iie.[AppearanceSet_Identity],
		0.5 * sum(1 - sign(iie.[Certainty])) + sum(iie.[Certainty]) as ConcurringCertainty
	from
		bitemporalTimepoints t
	cross apply 
		[Information_in_Effect](t.[AppearanceTime], t.[AssertionTime]) iie
	group by
		t.[AppearanceTime],
		t.[AssertionTime],
		iie.[Asserter_Identity],
		iie.[AppearanceSet_Identity]  
) ineq;
go

-- have contradictory assertions been made?
select * from [Check_for_Contradictions] order by AssertionTime desc, Asserter_Identity;

-- let's look at the contradiction
select * 
from [Information_in_Effect]('2018-12-01 12:00', '2018-12-13 15:35') 
where Asserter_Identity = 43;

-- which information is in effect at 12 and 13 given latest assertions?
-- it's still contradictory
select * 
from [Information_in_Effect]('2018-12-01 12:00', getdate())
where Asserter_Identity = 43;

-- Bella realizes she will remain forever contradictory about 12:00, 
-- unless she makes another assertion
exec AddPosit '60013 [{(60005, posit), (43, ascertains)}, "0.95", 2018-12-13 15:40]';

-- which information is in effect at 12 and 13 given latest assertions?
select * 
from [Information_in_Effect]('2018-12-01 12:00', getdate())
where Asserter_Identity = 43;

-- we haven't lost the information that Bella was at one point contradictory
select * from [Check_for_Contradictions] order by AssertionTime desc, Asserter_Identity;

-- but is anyone contradicting themselves according to the latest information we have?
select * from [Check_for_Contradictions] 
where AssertionTime = (
    select max(AssertionTime) from v_Assertion
)
order by AssertionTime desc, Asserter_Identity;

-- the Disagreer is at the same time changing their mind and retracts the previous statements
-- a retraction has reliability = 0
exec AddPosit '60014 [{(60007, posit), (46, ascertains)}, "0", 2018-12-13 15:40]';
exec AddPosit '60015 [{(60005, posit), (46, ascertains)}, "0", 2018-12-13 15:40]';

-- which information is now in effect at 12.00 and 13.00 at the time of the retraction? 
-- the Disagreer is without an opinion altogether
select * from [Information_in_Effect]('2018-12-01 12:00', '2018-12-13 15:40');
select * from [Information_in_Effect]('2018-12-01 13:00', '2018-12-13 15:40');

-- Bella is going to reiterate that Archie's beard was also shaved clean at 14:00
exec AddPosit '60016 [{(42, has beard)}, "shaved clean", 2018-12-01 14:00]';
exec AddPosit '60017 [{(60016, posit), (43, ascertains)}, "1.0", 2018-12-13 15:45]';

-- the latest assertion is called a "restatement", since the value is not changing
-- and the certainty is the same
select * from [v_Assertion] 
where Asserter_Identity = 43 
and AppearingValue.value('.', 'varchar(max)') = 'shaved clean'
order by AppearanceTime desc;

-- the information in effect will show the latest restatement 
select * from [Information_in_Effect]('2018-12-01 12:00', '2018-12-13 15:45');
select * from [Information_in_Effect]('2018-12-01 13:00', '2018-12-13 15:45');
-- it hides the fact that nothing actually changed since 13:00
select * from [Information_in_Effect]('2018-12-01 14:00', '2018-12-13 15:45');

-- Bella is going to reassure that she still is certain Archie's beard was shaved clean at 14
exec AddPosit '60018 [{(60016, posit), (43, ascertains)}, "1.0", 2018-12-13 15:50]';

-- the latest assertion is called a "reassertion", since only the assertion time changes
select * from [v_Assertion] 
where Asserter_Identity = 43 
and AppearingValue.value('.', 'varchar(max)') = 'shaved clean'
order by AppearanceTime desc, AssertionTime desc;

-- the information in effect will show the latest restatement and latest reassertion
-- it again hides the fact that nothing actually changed since 13:00
select * from [Information_in_Effect]('2018-12-01 14:00', '2018-12-13 15:45');
select * from [Information_in_Effect]('2018-12-01 14:00', '2018-12-13 15:50');


------------------------------------------------------------------------------------------------

select * from [Thing];
select * from [Role];
select * from [Appearance];
select * from [AppearanceSet];
select * from [Appearance_in_AppearanceSet];
select * from [Posit];
select * from [v_Posit];
select * from [v_Assertion];

----------------------------------------- TODO BELOW -------------------------------------------



--------------------------------------- METADATA ---------------------------------------
-- The good thing about posits, assertions, and so on being "things" in themselves
-- is that it makes it possible to produce posits about them as well. This is 
-- usually called metadata.

-- let the Script positor assert when what we have done so far was recorded in 
-- our database right now.

-- we now have 21 things
select * from [Thing];

-- create another role thing 
drop table if exists #Role2;
create table #Role2([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #Role2 values (DEFAULT);

-- create a new role
insert into [Role]([RoleUID], [Role]) 
select [UID], 'was recorded at' 
from #Role2;

-- check what the role looks like
select * from [Role];

-- create an appearance thing for every thing we have created
drop table if exists #Appearance2;
create table #Appearance2([UID] uniqueidentifier not null primary key); 
merge [Thing] t 
using [Thing] src on 1 = 0 -- make sure everything is "not matched" below
when not matched then insert ([UID]) values (DEFAULT)
output inserted.[UID] into #Appearance2;

-- create an equal number of appearances 
insert into [Appearance]([AppearanceUID], [ThingUID], [RoleUID])
select app.[UID], t.[UID], r.[RoleUID] 
from (select [UID], row_number() over (order by [UID]) as _row from #Appearance2) app
join (select [UID], row_number() over (order by [UID]) as _row from [Thing]) t 
on t._row = app._row
cross apply (select [RoleUID] from [Role] where [Role] = 'was recorded at') r;

-- we now have 23 appearances
select * from [Appearance];

-- create 22 dereferencing set things
drop table if exists #DereferencingSet2;
create table #DereferencingSet2([UID] uniqueidentifier not null primary key); 
merge [Thing] t 
using #Appearance2 src on 1 = 0 -- make sure everything is "not matched" below
when not matched then insert ([UID]) values (DEFAULT)
output inserted.[UID] into #DereferencingSet2;

-- create 22 dereferencing sets
insert into [DereferencingSet]([DereferencingSetUID]) 
select [UID]
from #DereferencingSet2;

-- add the appearances to the dereferencing sets
insert into [Dereference]([DereferencingSetUID], [AppearanceUID])
select s.[UID], app.[UID]
from (select [UID], row_number() over (order by [UID]) as _row from #DereferencingSet2) s
join (select [UID], row_number() over (order by [UID]) as _row from #Appearance2) app
on s._row = app._row;

-- create 22 posit things
drop table if exists #Posit5;
create table #Posit5([UID] uniqueidentifier not null primary key);
merge [Thing] t 
using #DereferencingSet2 src on 1 = 0 -- make sure everything is "not matched" below
when not matched then insert ([UID]) values (DEFAULT)
output inserted.[UID] into #Posit5;

-- create 22 posits (with the limitation that the date needs to be converted to a string)
insert into [Posit]([PositUID], [DereferencingSetUID], [Value], [AppearanceTime])
select p.[UID], s.[UID], cast(getdate() as varchar(max)), getdate()
from (select [UID], row_number() over (order by [UID]) as _row from #Posit5) p
join (select [UID], row_number() over (order by [UID]) as _row from #DereferencingSet2) s
on p._row = s._row;

-- we now have 26 posits, the last 22 of which intends to capture metadata
select * from [v_Posit];

-- finally let the Script positor assert these
drop table if exists #Assertion10;
create table #Assertion10([UID] uniqueidentifier not null primary key);
merge [Thing] t 
using #Posit5 src on 1 = 0 -- make sure everything is "not matched" below
when not matched then insert ([UID]) values (DEFAULT)
output inserted.[UID] into #Assertion10;

insert into [Assertion]([AssertionUID], [PositorUID], [PositUID], [Reliability], [AssertionTime])
select a.[UID], Script.[UID], p.[UID], 1, getdate()
from (select [UID], row_number() over (order by [UID]) as _row from #Assertion10) a
join (select [UID], row_number() over (order by [UID]) as _row from #Posit5) p
on a._row = p._row
cross apply #S Script;

-- there are now 31 assertions
select * from [v_Assertion] 
order by [DereferencingSetUID], [PositorUID], [AppearanceTime] desc, [AssertionTime] desc;

-- note that the metadata is not in effect if we travel back in time
select * from [Information_in_Effect]('2018-12-01 14:00', '2018-12-13 15:50')
order by [DereferencingSetUID], [PositorUID], [AppearanceTime] desc, [AssertionTime] desc;

-- finding all (current) metadata can be done by a condition on our Script positor
select * from [Information_in_Effect](getdate(), getdate())
where [PositorUID] = (select top 1 [UID] from #S)
order by [DereferencingSetUID], [PositorUID], [AppearanceTime] desc, [AssertionTime] desc;

--------------------------------------- RELATIONSHIPS ---------------------------------------
-- we will start by marrying Archie and Bella

-- create a few new role things
drop table if exists #Role3;
create table #Role3([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #Role3 values (DEFAULT);
drop table if exists #Role4;
create table #Role4([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #Role4 values (DEFAULT);
drop table if exists #Role5;
create table #Role5([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #Role5 values (DEFAULT);

-- create the new roles
insert into [Role]([RoleUID], [Role]) 
select [UID], 'husband' 
from #Role3;
insert into [Role]([RoleUID], [Role]) 
select [UID], 'wife' 
from #Role4;
insert into [Role]([RoleUID], [Role]) 
select [UID], 'church' 
from #Role5;

-- what roles have got now?
select * from [Role];

-- we will need a church thing
drop table if exists #C;
create table #C([UID] uniqueidentifier not null primary key); -- Church

-- create the Church thing
insert into [Thing]([UID]) output inserted.[UID] into #C values (DEFAULT);

-- we also need three appearance things
drop table if exists #Appearance3;
create table #Appearance3([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #Appearance3 values (DEFAULT);
drop table if exists #Appearance4;
create table #Appearance4([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #Appearance4 values (DEFAULT);
drop table if exists #Appearance5;
create table #Appearance5([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #Appearance5 values (DEFAULT);

-- create three appearances
insert into [Appearance]([AppearanceUID], [ThingUID], [RoleUID])
select app.[UID], Archie.[UID], r.[RoleUID] 
from [Role] r, #Appearance3 app, #A Archie 
where r.[Role] = 'husband';
insert into [Appearance]([AppearanceUID], [ThingUID], [RoleUID])
select app.[UID], Bella.[UID], r.[RoleUID] 
from [Role] r, #Appearance4 app, #B Bella 
where r.[Role] = 'wife';
insert into [Appearance]([AppearanceUID], [ThingUID], [RoleUID])
select app.[UID], Church.[UID], r.[RoleUID] 
from [Role] r, #Appearance5 app, #C Church
where r.[Role] = 'church';

-- create a dereferencing set thing
drop table if exists #DereferencingSet3;
create table #DereferencingSet3([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #DereferencingSet3 values (DEFAULT);

-- create a dereferencing set
insert into [DereferencingSet]([DereferencingSetUID]) 
select [UID]
from #DereferencingSet3;

-- add the created appearances to the (same) dereferencing set
insert into [Dereference]([DereferencingSetUID], [AppearanceUID])
select s.[UID], app.[UID]
from #DereferencingSet3 s, #Appearance3 app;
insert into [Dereference]([DereferencingSetUID], [AppearanceUID])
select s.[UID], app.[UID]
from #DereferencingSet3 s, #Appearance4 app;
insert into [Dereference]([DereferencingSetUID], [AppearanceUID])
select s.[UID], app.[UID]
from #DereferencingSet3 s, #Appearance5 app;

-- create a posit thing
drop table if exists #Posit6;
create table #Posit6([UID] uniqueidentifier not null primary key);
insert into [Thing]([UID]) output inserted.[UID] into #Posit6 values (DEFAULT);

-- create a posit
insert into [Posit]([PositUID], [DereferencingSetUID], [Value], [AppearanceTime])
select p.[UID], s.[UID], 'married', '2004-06-19 15:00'
from #Posit6 p, #DereferencingSet3 s;

-- now the posit is complete (note that the set has three members now)
select * from [v_Posit] 
where [DereferencingSetUID] = (select top 1 [UID] from #DereferencingSet3);

-- let Bella assert this, so first create an assertion thing
drop table if exists #Assertion11;
create table #Assertion11([UID] uniqueidentifier not null primary key);
insert into [Thing]([UID]) output inserted.[UID] into #Assertion11 values (DEFAULT);

-- create an assertion
insert into [Assertion]([AssertionUID], [PositorUID], [PositUID], [Reliability], [AssertionTime])
select a.[UID], Bella.[UID], p.[UID], 1, '2018-12-13 15:55'
from #Assertion11 a, #B Bella, #Posit6 p;

-- now Bella has asserted the marriage
select * from [v_Assertion] 
where [DereferencingSetUID] = (select top 1 [UID] from #DereferencingSet3)
order by [DereferencingSetUID], [PositorUID], [AppearanceTime] desc, [AssertionTime] desc;

-- let them divorce a while later (using the same dereferencing set)
-- create a posit thing
drop table if exists #Posit7;
create table #Posit7([UID] uniqueidentifier not null primary key);
insert into [Thing]([UID]) output inserted.[UID] into #Posit7 values (DEFAULT);

-- create a posit
insert into [Posit]([PositUID], [DereferencingSetUID], [Value], [AppearanceTime])
select p.[UID], s.[UID], 'divorced', '2010-01-31 10:00'
from #Posit7 p, #DereferencingSet3 s;

-- the state of this dereferencing set has transitioned from 'married' to 'divorced'
select * from [v_Posit] 
where [DereferencingSetUID] = (select top 1 [UID] from #DereferencingSet3);

-- let Bella assert this as well, so first create an assertion thing
drop table if exists #Assertion12;
create table #Assertion12([UID] uniqueidentifier not null primary key);
insert into [Thing]([UID]) output inserted.[UID] into #Assertion12 values (DEFAULT);

-- create an assertion
insert into [Assertion]([AssertionUID], [PositorUID], [PositUID], [Reliability], [AssertionTime])
select a.[UID], Bella.[UID], p.[UID], 1, '2018-12-13 15:55'
from #Assertion12 a, #B Bella, #Posit7 p;

-- now Bella has asserted the divorce
select * from [v_Assertion] 
where [DereferencingSetUID] = (select top 1 [UID] from #DereferencingSet3)
order by [DereferencingSetUID], [PositorUID], [AppearanceTime] desc, [AssertionTime] desc;

-- however, as it turns out, Bella remarried the Disagreer at a later time, but not in a church
-- that means a new dereferencing set is needed and this time without the church

-- first the Disagreer must appear as husband
drop table if exists #Appearance6;
create table #Appearance6([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #Appearance6 values (DEFAULT);

insert into [Appearance]([AppearanceUID], [ThingUID], [RoleUID])
select app.[UID], Disagreer.[UID], r.[RoleUID] 
from [Role] r, #Appearance6 app, #D Disagreer 
where r.[Role] = 'husband';

-- create a dereferencing set thing
drop table if exists #DereferencingSet4;
create table #DereferencingSet4([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #DereferencingSet4 values (DEFAULT);

-- create a dereferencing set
insert into [DereferencingSet]([DereferencingSetUID]) 
select [UID]
from #DereferencingSet4;

-- add the created appearances to the dereferencing set (with only two members)
insert into [Dereference]([DereferencingSetUID], [AppearanceUID])
select s.[UID], app.[UID]
from #DereferencingSet4 s, #Appearance6 app;
insert into [Dereference]([DereferencingSetUID], [AppearanceUID])
select s.[UID], app.[UID]
from #DereferencingSet4 s, #Appearance4 app;

-- create a posit thing
drop table if exists #Posit8;
create table #Posit8([UID] uniqueidentifier not null primary key);
insert into [Thing]([UID]) output inserted.[UID] into #Posit8 values (DEFAULT);

-- create a posit
insert into [Posit]([PositUID], [DereferencingSetUID], [Value], [AppearanceTime])
select p.[UID], s.[UID], 'married', '2011-11-11 11:11'
from #Posit8 p, #DereferencingSet4 s;

-- let Bella assert this as well, so first create an assertion thing
drop table if exists #Assertion13;
create table #Assertion13([UID] uniqueidentifier not null primary key);
insert into [Thing]([UID]) output inserted.[UID] into #Assertion13 values (DEFAULT);

-- create an assertion
insert into [Assertion]([AssertionUID], [PositorUID], [PositUID], [Reliability], [AssertionTime])
select a.[UID], Bella.[UID], p.[UID], 1, '2018-12-13 15:55'
from #Assertion13 a, #B Bella, #Posit8 p;

-- now Bella has asserted her second marriage
-- we can find this from finding every assertion in which Bella has appeared in the wife role
-- regardless if the relationship involves a church or not
-- note: it is also possible to achieve the same result using joins instead of the XML query
select a.* from [v_Assertion] a cross apply #B Bella
where AssertionXML.exist('/Assertion/Posit/DereferencingSet/Appearance[@UID = sql:column("Bella.UID") and @Role = "wife"]') = 1
order by [PositorUID], [AppearanceTime] desc, [AssertionTime] desc;

-- but, Bella made a mistake, the appearance time of her second marriage is not correct
-- so she first makes a retraction of the erroneous assertion
drop table if exists #Assertion14;
create table #Assertion14([UID] uniqueidentifier not null primary key);
insert into [Thing]([UID]) output inserted.[UID] into #Assertion14 values (DEFAULT);

-- create an assertion (that is a retraction since Reliability = 0)
insert into [Assertion]([AssertionUID], [PositorUID], [PositUID], [Reliability], [AssertionTime])
select a.[UID], Bella.[UID], p.[UID], 0, '2018-12-13 16:00'
from #Assertion14 a, #B Bella, #Posit8 p;

-- in the view of all assertions the retraction is now visible
select a.* from [v_Assertion] a cross apply #B Bella
where AssertionXML.exist('/Assertion/Posit/DereferencingSet/Appearance[@UID = sql:column("Bella.UID") and @Role = "wife"]') = 1
order by [PositorUID], [AppearanceTime] desc, [AssertionTime] desc;

-- which means that the latest information we have is that Bella is divorced
select a.* from [Information_in_Effect](getdate(), getdate()) a cross apply #B Bella
where AssertionXML.exist('/Assertion/Posit/DereferencingSet/Appearance[@UID = sql:column("Bella.UID") and @Role = "wife"]') = 1
order by [PositorUID], [AppearanceTime] desc, [AssertionTime] desc;

-- so Bella needs to assert a different posit, with the correct appearance time
drop table if exists #Posit9;
create table #Posit9([UID] uniqueidentifier not null primary key);
insert into [Thing]([UID]) output inserted.[UID] into #Posit9 values (DEFAULT);

insert into [Posit]([PositUID], [DereferencingSetUID], [Value], [AppearanceTime])
select p.[UID], s.[UID], 'married', '2012-12-12 12:12'
from #Posit9 p, #DereferencingSet4 s;

drop table if exists #Assertion15;
create table #Assertion15([UID] uniqueidentifier not null primary key);
insert into [Thing]([UID]) output inserted.[UID] into #Assertion15 values (DEFAULT);

insert into [Assertion]([AssertionUID], [PositorUID], [PositUID], [Reliability], [AssertionTime])
select a.[UID], Bella.[UID], p.[UID], 1, '2018-12-13 16:00'
from #Assertion15 a, #B Bella, #Posit9 p;

-- in the view of all assertions the correction is now visible
select a.* from [v_Assertion] a cross apply #B Bella
where AssertionXML.exist('/Assertion/Posit/DereferencingSet/Appearance[@UID = sql:column("Bella.UID") and @Role = "wife"]') = 1
order by [PositorUID], [AppearanceTime] desc, [AssertionTime] desc;

-- new the latest information we have is that Bella is married, but note that the earlier divorce is also shown
-- due to it having a different dereferencing set
select a.* from [Information_in_Effect](getdate(), getdate()) a cross apply #B Bella
where AssertionXML.exist('/Assertion/Posit/DereferencingSet/Appearance[@UID = sql:column("Bella.UID") and @Role = "wife"]') = 1
order by [PositorUID], [AppearanceTime] desc, [AssertionTime] desc;


------------------------------------------- MODELING ------------------------------------------
/*
----------- Excerpt from: Modeling Conflicting, Unreliable, and Varying Information -----------
def. of a classifier and a class
Let "is class" be a role reserved for the purpose of modeling. 
A posit pc = [{(C, is class)}, c, t], defines the name
of a class through the string c and associates the unique
identifier C with it. A classifier is a relationship that binds
a thing to a class, expressed through posits on the form
pM = [{(i, thing),(C, class)}, v, t].
-----------------------------------------------------------------------------------------------

It is time for the Modeler to step in and tell us what some of our things are.
*/
-- we need three new roles in order to create a model
drop table if exists #Role6;
create table #Role6([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #Role6 values (DEFAULT);
drop table if exists #Role7;
create table #Role7([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #Role7 values (DEFAULT);
drop table if exists #Role8;
create table #Role8([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #Role8 values (DEFAULT);

-- create the new roles
insert into [Role]([RoleUID], [Role]) 
select [UID], 'is class' 
from #Role6;
insert into [Role]([RoleUID], [Role]) 
select [UID], 'thing' 
from #Role7;
insert into [Role]([RoleUID], [Role]) 
select [UID], 'class' 
from #Role8;

-- what roles have got now?
select * from [Role] order by RoleUID;

-- create two class things
drop table if exists #Class1;
create table #Class1([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #Class1 values (DEFAULT);
drop table if exists #Class2;
create table #Class2([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #Class2 values (DEFAULT);

-- the classes need names, so first they need to appear with the "is class" role
drop table if exists #Appearance7;
create table #Appearance7([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #Appearance7 values (DEFAULT);
drop table if exists #Appearance8;
create table #Appearance8([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #Appearance8 values (DEFAULT);

insert into [Appearance]([AppearanceUID], [ThingUID], [RoleUID])
select app.[UID], c.[UID], r.[RoleUID] 
from [Role] r, #Appearance7 app, #Class1 c 
where r.[Role] = 'is class';
insert into [Appearance]([AppearanceUID], [ThingUID], [RoleUID])
select app.[UID], c.[UID], r.[RoleUID] 
from [Role] r, #Appearance8 app, #Class2 c 
where r.[Role] = 'is class';

-- create a dereferencing set thing
drop table if exists #DereferencingSet5;
create table #DereferencingSet5([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #DereferencingSet5 values (DEFAULT);
drop table if exists #DereferencingSet6;
create table #DereferencingSet6([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #DereferencingSet6 values (DEFAULT);

-- create dereferencing sets
insert into [DereferencingSet]([DereferencingSetUID]) 
select [UID]
from #DereferencingSet5;
insert into [DereferencingSet]([DereferencingSetUID]) 
select [UID]
from #DereferencingSet6;

-- add the created appearances
insert into [Dereference]([DereferencingSetUID], [AppearanceUID])
select s.[UID], app.[UID]
from #DereferencingSet5 s, #Appearance7 app;
insert into [Dereference]([DereferencingSetUID], [AppearanceUID])
select s.[UID], app.[UID]
from #DereferencingSet6 s, #Appearance8 app;

-- two posit things
drop table if exists #Posit10;
create table #Posit10([UID] uniqueidentifier not null primary key);
insert into [Thing]([UID]) output inserted.[UID] into #Posit10 values (DEFAULT);
drop table if exists #Posit11;
create table #Posit11([UID] uniqueidentifier not null primary key);
insert into [Thing]([UID]) output inserted.[UID] into #Posit11 values (DEFAULT);

-- two posits
insert into [Posit]([PositUID], [DereferencingSetUID], [Value], [AppearanceTime])
select p.[UID], s.[UID], 'Person', '1901-01-01' -- will represent dawn of time
from #Posit10 p, #DereferencingSet5 s;
insert into [Posit]([PositUID], [DereferencingSetUID], [Value], [AppearanceTime])
select p.[UID], s.[UID], 'Cathedral', '1901-01-01' -- will represent dawn of time
from #Posit11 p, #DereferencingSet6 s;

-- now, the Modeler must assert these
drop table if exists #Assertion16;
create table #Assertion16([UID] uniqueidentifier not null primary key);
insert into [Thing]([UID]) output inserted.[UID] into #Assertion16 values (DEFAULT);
drop table if exists #Assertion17;
create table #Assertion17([UID] uniqueidentifier not null primary key);
insert into [Thing]([UID]) output inserted.[UID] into #Assertion17 values (DEFAULT);

insert into [Assertion]([AssertionUID], [PositorUID], [PositUID], [Reliability], [AssertionTime])
select a.[UID], Modeler.[UID], p.[UID], 1, '2018-12-01 00:00'
from #Assertion16 a, #M Modeler, #Posit10 p;
insert into [Assertion]([AssertionUID], [PositorUID], [PositUID], [Reliability], [AssertionTime])
select a.[UID], Modeler.[UID], p.[UID], 1, '2018-12-01 00:00'
from #Assertion17 a, #M Modeler, #Posit11 p;

-- list all classes
select a.* from [v_Assertion] a 
where AssertionXML.exist('/Assertion/Posit/DereferencingSet/Appearance[@Role = "is class"]') = 1
order by [PositorUID], [AppearanceTime] desc, [AssertionTime] desc;

-- lot's of work, but two classes are defined
-- they have their own unique identifiers, so many more roles can be added to provide additional
-- information about the classes

-- now it is time to associate some things with the classes
-- every such thing needs an appearance with the 'thing' role
drop table if exists #Appearance9;
create table #Appearance9([UID] uniqueidentifier not null primary key); 
merge [Thing] t 
using (
	select [UID] from #A
	union all
	select [UID] from #B
	union all
	select [UID] from #C
	union all
	select [UID] from #M
	union all
	select [UID] from #D
) src 
on 1 = 0 -- make sure everything is "not matched" below
when not matched then insert ([UID]) values (DEFAULT)
output inserted.[UID] into #Appearance9;

-- create an equal number of appearances 
insert into [Appearance]([AppearanceUID], [ThingUID], [RoleUID])
select app.[UID], t.[UID], r.[RoleUID] 
from (select [UID], row_number() over (order by [UID]) as _row from #Appearance9) app
join (select [UID], row_number() over (order by [UID]) as _row from (
	select [UID] from #A
	union all
	select [UID] from #B
	union all
	select [UID] from #C
	union all
	select [UID] from #M
	union all
	select [UID] from #D
) things ) t 
on t._row = app._row
cross apply (select [RoleUID] from [Role] where [Role] = 'thing') r;

-- then two more appearances (one for each associated class)
drop table if exists #Appearance10;
create table #Appearance10([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #Appearance10 values (DEFAULT);
drop table if exists #Appearance11;
create table #Appearance11([UID] uniqueidentifier not null primary key); 
insert into [Thing]([UID]) output inserted.[UID] into #Appearance11 values (DEFAULT);

insert into [Appearance]([AppearanceUID], [ThingUID], [RoleUID])
select app.[UID], c.[UID], r.[RoleUID] 
from [Role] r, #Appearance10 app, #Class1 c 
where r.[Role] = 'class';
insert into [Appearance]([AppearanceUID], [ThingUID], [RoleUID])
select app.[UID], c.[UID], r.[RoleUID] 
from [Role] r, #Appearance11 app, #Class2 c 
where r.[Role] = 'class';

-- create dereferencing set things (equal in number to the things we want to classify)
drop table if exists #DereferencingSet7;
create table #DereferencingSet7([UID] uniqueidentifier not null primary key); 
merge [Thing] t 
using #Appearance9 src on 1 = 0 -- make sure everything is "not matched" below
when not matched then insert ([UID]) values (DEFAULT)
output inserted.[UID] into #DereferencingSet7;

-- create 22 dereferencing sets
insert into [DereferencingSet]([DereferencingSetUID]) 
select [UID]
from #DereferencingSet7;

-- add the appearances to the dereferencing sets
-- first the things
insert into [Dereference]([DereferencingSetUID], [AppearanceUID])
select s.[UID], app.[UID]
from (select [UID], row_number() over (order by [UID]) as _row from #DereferencingSet7) s
join (select [UID], row_number() over (order by [UID]) as _row from #Appearance9) app
on s._row = app._row;
-- then the classes
insert into [Dereference]([DereferencingSetUID], [AppearanceUID])
select 
	s.[UID], 
		case 
			when exists (
					select top 1 d.[DereferencingSetUID]
					from [Dereference] d 
					join [Appearance] a
					  on a.[AppearanceUID] = d.[AppearanceUID]
					join (
						select [UID] from #A
						union all
						select [UID] from #B
						union all
						select [UID] from #M
						union all
						select [UID] from #D
					) persons
					on persons.[UID] = a.[ThingUID]
					where d.[DereferencingSetUID] = s.[UID]
			) then Person.[UID]
			else Cathedral.[UID]
		end
from #DereferencingSet7 s, #Appearance10 Person, #Appearance11 Cathedral;

-- create posit things
drop table if exists #Posit12;
create table #Posit12([UID] uniqueidentifier not null primary key);
merge [Thing] t 
using #DereferencingSet7 src on 1 = 0 -- make sure everything is "not matched" below
when not matched then insert ([UID]) values (DEFAULT)
output inserted.[UID] into #Posit12;

-- create posits 
insert into [Posit]([PositUID], [DereferencingSetUID], [Value], [AppearanceTime])
select p.[UID], s.[UID], 'active', '1901-01-01'
from (select [UID], row_number() over (order by [UID]) as _row from #Posit12) p
join (select [UID], row_number() over (order by [UID]) as _row from #DereferencingSet7) s
on p._row = s._row;

-- finally let the Modeler positor assert these
drop table if exists #Assertion18;
create table #Assertion18([UID] uniqueidentifier not null primary key);
merge [Thing] t 
using #Posit12 src on 1 = 0 -- make sure everything is "not matched" below
when not matched then insert ([UID]) values (DEFAULT)
output inserted.[UID] into #Assertion18;

insert into [Assertion]([AssertionUID], [PositorUID], [PositUID], [Reliability], [AssertionTime])
select a.[UID], Modeler.[UID], p.[UID], 1, '2018-12-01 00:00'
from (select [UID], row_number() over (order by [UID]) as _row from #Assertion18) a
join (select [UID], row_number() over (order by [UID]) as _row from #Posit12) p
on a._row = p._row
cross apply #M Modeler;

-- list all classes and classifiers
-- as can be seen there are four things of class Person and one thing of class Cathedral
select a.* from [v_Assertion] a 
where AssertionXML.exist('/Assertion/Posit/DereferencingSet/Appearance[@Role = "is class" or @Role = "class"]') = 1
order by [PositorUID], [AppearanceTime] desc, [AssertionTime] desc;


/*
----------- Excerpt from: Modeling Conflicting, Unreliable, and Varying Information -----------
def. of a posit type
A posit type, τ(p) = [{(C1,r1), . . . ,(Cn,rn)}, τ(v), τ(t)], for a
posit p = [{(i1,r1), . . . ,(in,rn)}, v, t], is a structure constructed
by replacing unique identifiers, ij with the unique identifiers of their 
class, Cj, the value, v, with its data type, τ(v), and the time point, t, 
with its data type, τ(t).
-----------------------------------------------------------------------------------------------

A posit type is a structure, but posits in the relational model can only have 
values of type varchar(max), so it cannot hold such a structure. It could either
be expressed as serialized XML or JSON, or we could let the field contain a 
reference to an identifier in a separate table that replicates the structure.
Since a posit type contains a set with variable number of members, it is 
easier in this case to express it using XML.
*/