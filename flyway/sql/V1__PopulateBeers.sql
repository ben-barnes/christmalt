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

create table history (
  id serial primary key
, beer integer not null references beers
, consumed date not null
);

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
end;
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

select add_beer('Lamington Ale', 'Quiet Deeds', 'Brown Ale', 5.5);
select add_beer('Elvis Juice', 'BrewDog', 'American IPA', 6.5);
select add_beer('Former Tenant', 'Modus Operandi', 'Red IPA', 7.8);
select add_beer('Archie''s Red Ale', 'Mismatch Brewing Co', 'Red Ale', 5.0);
select add_beer('I.P.A. Australia', 'Colonial Brewing Co', 'Australian IPA', 6.5);
select add_beer('Hop culture', 'Mornington Peninsula Brewery', 'Session IPA', 4.9);
select add_beer('Long Reef Pale Ale', 'Nomad Brewing Co', 'American Pale Ale', 5.0);
select add_beer('Passion Fruit Pinner', 'Oskar Blues Brewery', 'Session IPA', 4.9);
select add_beer('IPA', 'Pirate Life Brewing', 'IPA', 6.8);
select add_beer('Rundy Bum', 'Red Duck', 'Old Ale', 8.2);
select add_beer('8 Bit IPA', 'Stockade Brew Co', 'American IPA', 6.5);
select add_beer('Stout', '4 Pines Brewing Co', 'Dry Irish Stout', 5.1);
select add_beer('Breakfast Stout', 'Founders Brewing Co', 'Imperial Stout', 8.3);
select add_beer('Grapefruit Sculpin', 'Ballast Point Brewing Co', 'IPA', 7.0);
select add_beer('Gunnamatta', 'Yeastie Boys', 'American IPA', 6.5);
select add_beer('Jet Lag IPA', 'Nomad Brewing Co', 'IPA', 6.2);
select add_beer('Pinner Throwback IPA', 'Oskar Blues Brewery', 'Session IPA', 4.9);
select add_beer('Lager', 'Mornington Peninsula Brewery', 'German Lager', 4.7);
select add_beer('Coastie Pale Lager', 'Six String Brewing Company', 'Pale Lager', 5.0);
select add_beer('Evil Twin', 'Heretic Brewing Company', 'American Strong Ale', 6.8);
select add_beer('Alt Brown', 'Balter Brewing Company', 'Altbier', 5.2);
select add_beer('Golden Stout Time', 'Big Shed Brewing Concern', 'Sweet Stout', 5.4);
select add_beer('Union''Hop', 'Moor Beer Co', 'English Pale Ale', 4.1);
select add_beer('Golden Monkey', 'Victory Brewing Company', 'Tripel', 9.5);
