create table breweries (
  id serial primary key
, name varchar(255) unique not null
, location varchar(255) not null
);

create table styles (
  id serial primary key
, name varchar(255) unique not null
);

create table beers (
  id serial primary key
, name varchar(255) not null
, brewery integer not null references breweries
, style integer not null references styles
, abv numeric(2, 1) not null
);

create table numbering (
  code int primary key
, id integer not null references beers
);

create table permutation (
  old int primary key
, new int not null unique
);

create table history (
  position int primary key,
  consumeOn date not null unique
);

create view details as
select
  beers.name as name
, breweries.name as brewery
, breweries.location as location
, styles.name as style
, beers.abv as abv
, history.consumeOn as consumeOn
from
  beers
  join breweries on beers.brewery = breweries.id
  join styles on beers.style = styles.id
  join numbering on beers.id = numbering.id
  join permutation on numbering.code = permutation.new
  join history on permutation.old = history.position;

create or replace function add_brewery(b_name varchar(255), b_location varchar(255))
returns void as $$
begin
  insert into breweries(name, location) values (b_name, b_location);
end;
$$ language plpgsql;

create or replace function add_style(s_name varchar(255))
returns void as $$
begin
  insert into styles(name) values (s_name);
end;
$$ language plpgsql;

create or replace function add_beer(
  b_number int,
  b_name varchar(255)
, b_brewery varchar(255)
, b_style varchar(255)
, b_abv numeric(2, 1)
)
returns void as $$
begin
  insert into beers(name, brewery, style, abv) values (
    b_name
  , (select id from breweries where name = b_brewery)
  , (select id from styles where name = b_style)
  , b_abv
  );
  insert into numbering values (
    b_number,
    ( select id from beers where name = b_name )
  );
end;
$$ language plpgsql;

create or replace function add_permutation(
  p_old int,
  p_new int
)
returns void as $$
begin
  insert into permutation values (p_old, p_new);
end;
$$ language plpgsql;

create or replace function add_history(
  h_position int
, h_consumeOn date
)
returns void as $$
begin
  insert into history values (h_position, h_consumeOn);
end
$$ language plpgsql;

select add_brewery('Quiet Deeds', 'Victoria, Australia');
select add_brewery('BrewDog', 'Scotland, UK');
select add_brewery('Modus Operandi', 'NSW, Australia');
select add_brewery('Mismatch Brewing Co', 'South Australia');
select add_brewery('Colonial Brewing Co', 'Western Australia');
select add_brewery('Mornington Peninsula Brewery', 'Victoria, Australia');
select add_brewery('Nomad Brewing Co', 'NSW, Australia');
select add_brewery('Oskar Blues Brewery', 'Colorado, USA');
select add_brewery('Pirate Life Brewing', 'South Australia');
select add_brewery('Red Duck', 'Victoria, Australia');
select add_brewery('Stockade Brew Co', 'NSW, Australia');
select add_brewery('4 Pines Brewing Co', 'NSW, Australia');
select add_brewery('Founders Brewing Co', 'Michigan, USA');
select add_brewery('Ballast Point Brewing Co', 'California, USA');
select add_brewery('Yeastie Boys', 'Wellington, New Zealand');
select add_brewery('Six String Brewing Company', 'NSW, Australia');
select add_brewery('Heretic Brewing Company', 'California, USA');
select add_brewery('Balter Brewing Company', 'QLD, Australia');
select add_brewery('Big Shed Brewing Concern', 'South Australia');
select add_brewery('Moor Beer Co', 'Bristol, UK');
select add_brewery('Victory Brewing Company', 'Pennsylvania, USA');

select add_style('Brown Ale');
select add_style('American IPA');
select add_style('Red IPA');
select add_style('Red Ale');
select add_style('Australian IPA');
select add_style('Session IPA');
select add_style('American Pale Ale');
select add_style('IPA');
select add_style('Old Ale');
select add_style('Dry Irish Stout');
select add_style('Imperial Stout');
select add_style('German Lager');
select add_style('Pale Lager');
select add_style('American Strong Ale');
select add_style('Altbier');
select add_style('Sweet Stout');
select add_style('English Pale Ale');
select add_style('Tripel');

select add_beer(8, 'Lamington Ale', 'Quiet Deeds', 'Brown Ale', 5.5);
select add_beer(19, 'Elvis Juice', 'BrewDog', 'American IPA', 6.5);
select add_beer(24, 'Former Tenant', 'Modus Operandi', 'Red IPA', 7.8);
select add_beer(4, 'Archie''s Red Ale', 'Mismatch Brewing Co', 'Red Ale', 5.0);
select add_beer(15, 'I.P.A. Australia', 'Colonial Brewing Co', 'Australian IPA', 6.5);
select add_beer(2, 'Hop culture', 'Mornington Peninsula Brewery', 'Session IPA', 4.9);
select add_beer(18, 'Long Reef Pale Ale', 'Nomad Brewing Co', 'American Pale Ale', 5.0);
select add_beer(13, 'Passion Fruit Pinner', 'Oskar Blues Brewery', 'Session IPA', 4.9);
select add_beer(7, 'IPA', 'Pirate Life Brewing', 'IPA', 6.8);
select add_beer(11, 'Rundy Bum', 'Red Duck', 'Old Ale', 8.2);
select add_beer(10, '8 Bit IPA', 'Stockade Brew Co', 'American IPA', 6.5);
select add_beer(6, 'Stout', '4 Pines Brewing Co', 'Dry Irish Stout', 5.1);
select add_beer(5, 'Breakfast Stout', 'Founders Brewing Co', 'Imperial Stout', 8.3);
select add_beer(1, 'Grapefruit Sculpin', 'Ballast Point Brewing Co', 'IPA', 7.0);
select add_beer(16, 'Gunnamatta', 'Yeastie Boys', 'American IPA', 6.5);
select add_beer(20, 'Jet Lag IPA', 'Nomad Brewing Co', 'IPA', 6.2);
select add_beer(9, 'Pinner Throwback IPA', 'Oskar Blues Brewery', 'Session IPA', 4.9);
select add_beer(21, 'Lager', 'Mornington Peninsula Brewery', 'German Lager', 4.7);
select add_beer(3, 'Coastie Pale Lager', 'Six String Brewing Company', 'Pale Lager', 5.0);
select add_beer(23, 'Evil Twin', 'Heretic Brewing Company', 'American Strong Ale', 6.8);
select add_beer(12, 'Alt Brown', 'Balter Brewing Company', 'Altbier', 5.2);
select add_beer(17, 'Golden Stout Time', 'Big Shed Brewing Concern', 'Sweet Stout', 5.4);
select add_beer(14, 'Union''Hop', 'Moor Beer Co', 'English Pale Ale', 4.1);
select add_beer(22, 'Golden Monkey', 'Victory Brewing Company', 'Tripel', 9.5);

select add_permutation(1, 24);
select add_permutation(2, 12);
select add_permutation(3, 16);
select add_permutation(4, 18);
select add_permutation(5, 14);
select add_permutation(6, 13);
select add_permutation(7, 19);
select add_permutation(8, 17);
select add_permutation(9, 20);
select add_permutation(10, 15);
select add_permutation(11, 21);
select add_permutation(12, 22);
select add_permutation(13, 23);
select add_permutation(14, 3);
select add_permutation(15, 9);
select add_permutation(16, 7);
select add_permutation(17, 11);
select add_permutation(18, 1);
select add_permutation(19, 6);
select add_permutation(20, 10);
select add_permutation(21, 2);
select add_permutation(22, 4);
select add_permutation(23, 8);
select add_permutation(24, 5);

select add_history(1, '2016-12-01');
select add_history(2, '2016-12-02');
select add_history(3, '2016-12-03');
select add_history(4, '2016-12-04');
select add_history(5, '2016-12-05');
select add_history(6, '2016-12-06');
select add_history(7, '2016-12-07');
select add_history(8, '2016-12-08');
select add_history(9, '2016-12-09');
select add_history(10, '2016-12-10');
select add_history(11, '2016-12-11');
select add_history(12, '2016-12-12');
select add_history(13, '2016-12-13');
select add_history(14, '2016-12-14');
select add_history(15, '2016-12-15');
select add_history(16, '2016-12-16');
select add_history(17, '2016-12-17');
select add_history(18, '2016-12-18');
select add_history(19, '2016-12-19');
select add_history(20, '2016-12-20');
select add_history(21, '2016-12-21');
select add_history(22, '2016-12-22');
select add_history(23, '2016-12-23');
select add_history(24, '2016-12-24');
