
drop database if exists datasciencetest;

create database datasciencetest;


connect datasciencetest;


create table sales(
Date date,
OrderNo varchar(10),
PartNo varchar(10),
ShopNo varchar(10),
Qty int);


create table purchase(
Date date,
PartNo varchar(10),
ShopNo varchar(10),
Qty int);


load data local infile 'C:/temp/in_sales.csv' into table sales
fields terminated by ','
enclosed by '"'
lines terminated by '\n';


load data local infile 'C:/temp/in_purchase.csv' into table purchase
fields terminated by ','
enclosed by '"'
lines terminated by '\n';


create table nsales(
Date date,
PartNo varchar(10),
ShopNo varchar(10),
Qty int);


create table npurchase(
Date date,
PartNo varchar(10),
ShopNo varchar(10),
Qty int);


insert into nsales(Date, PartNo, ShopNo, Qty)
select Date, PartNo, ShopNo, SUM(Qty) from sales
group by Date, PartNo, ShopNo;


insert into npurchase(Date, PartNo, ShopNo, Qty)
select Date, PartNo, ShopNo, SUM(Qty) from purchase
group by Date, PartNo, ShopNo;


#select * from nsales;
#select * from npurchase;


create table PartShopCartesian(
PartNo varchar(10),
ShopNo varchar(10));


insert into PartShopCartesian(PartNo, ShopNo)
select distinct(s1.PartNo), s2.ShopNo from sales s1
cross join (select distinct(ShopNo) as ShopNo from sales) s2;


create table Result(
Date date,
PartNo varchar(10),
ShopNo varchar(10),
Sales int,
CumulativeSales int,
Stock int);


insert into Result(Date, PartNo, ShopNo)
select a.Date, p.PartNo, P.ShopNo from PartShopCartesian p
cross join (
select a.Date
from (
select curdate() - INTERVAL (a.a + (10 * b.a) + (100 * c.a)) DAY as Date
from (select 0 as a union all select 1 union all select 2 union all select 3 union all select 4 union all select 5 union all select 6 union all select 7 union all select 8 union all select 9) as a
cross join (select 0 as a union all select 1 union all select 2 union all select 3 union all select 4 union all select 5 union all select 6 union all select 7 union all select 8 union all select 9) as b
cross join (select 0 as a union all select 1 union all select 2 union all select 3 union all select 4 union all select 5 union all select 6 union all select 7 union all select 8 union all select 9) as c
) a
where a.Date between '2015-01-01' and '2015-03-31') a;


alter table Result order by Result.PartNo, Result.ShopNo, Result.Date;


update Result r
inner join nsales s on (r.Date=s.Date and r.PartNo=s.PartNo and r.ShopNo=s.ShopNo)
set r.Sales = s.Qty;


update Result
set Sales=0
where Sales is null;


update Result r
set CumulativeSales = (
select sum(s.Qty) from nsales s
where r.Date>=s.Date and r.PartNo=s.PartNo and r.ShopNo=s.ShopNo
group by r.PartNo, r.ShopNo);


update Result
set CumulativeSales=0
where CumulativeSales is null;


update Result r
set Stock = (
select sum(p.Qty) - r.CumulativeSales from npurchase p
where r.Date>p.Date and r.PartNo=p.PartNo and r.ShopNo=p.ShopNo
group by r.PartNo, r.ShopNo);


select * from Result
INTO OUTFILE 'C:/temp/out.csv'
fields terminated by ','
enclosed by '"'
lines terminated by '\n';
