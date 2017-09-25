unit Udate;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
udate: Functions for manipulating dates. Note that our date system starts counting
at zero, and the Delphi date system starts counting at one. (Ours is left over from C++.)
The conversion functions take this into account, but watch out for it.}

interface

uses uestruct;

type

GsDate = record
  dateTime: TDateTime;
  reservedArray: array [1..4] of byte;
  end;

type
  monthArray = array[0..11] of integer;

const
  cumDaysInMonthNonLeapYear: monthArray = (29, 57, 88, 118, 149, 179, 210, 241, 271, 302, 332, 364);

const
  kJanuary = 0;
  kFebruary = 1;
  kMarch = 2;
  kApril = 3;
  kMay = 4;
  kJune = 5;
  kJuly = 6;
  kAugust = 7;
  kSeptember = 8;
  kOctober = 9;
  kNovember = 10;
  kDecember = 11;

{ initialization }
function GsDate_currentDate: GsDate;
function GsDate_dateFromYearMonthDayOfMonth(year, month, dayOfMonth: word): GsDate;

{ special encoding/decoding incorporating our indexes starting at zero }
procedure GsDate_decodeDate(date: GsDate; var year, month, dayOfMonth: word);
function GsDate_encodeDate(year, month, dayOfMonth: integer): GsDate;

{ incrementing }
procedure GsDate_addYears(var date: GsDate; changeInYears: integer);
procedure GsDate_addMonths(var date: GsDate; changeInMonths: integer);
procedure GsDate_addDays(var date: GsDate; changeInDays: integer);

{ date math using GsDates }
function GsDate_daysBetween(firstDate, secondDate: GsDate): longint;
function GsDate_daysInMonthFromDate(date: GsDate): integer;
function GsDate_daysInXMonthsForwardFromDate(date: GsDate; months: integer): integer;
function GsDate_firstDateIsEarlierThanSecond(firstDate, secondDate: GsDate): boolean;
function GsDate_firstDateIsLaterThanSecond(firstDate, secondDate: GsDate): boolean;
function GsDate_firstDateIsEarlierThanOrEqualToSecond(firstDate, secondDate: GsDate): boolean;
function GsDate_dayOfYearFromDate(date: GsDate): integer;
function GsDate_weekOfYearFromDate(date: GsDate): integer;
function GsDate_yearFromDate(date: GsDate): integer;
function GsDate_monthFromDate(date: GsDate): integer;
function GsDate_dayOfMonthFromDate(date: GsDate): integer;
function GsDate_isLeapYearFromDate(date: GsDate): boolean;

{ date math not using GsDates - for these, don't care if is leap year }
function GsDate_monthFromDayOfYear_NoLeap(dayOfYear: integer): integer;
function GsDate_daysInMonthFromMonth_NoLeap(month: integer): integer;
function GsDate_dayOfYearFromDayOfMonthAndMonth_NoLeap(dayOfMonth, month: integer): integer;
function GsDate_dayOfMonthFromDayOfYear_NoLeap(dayOfYear: integer): integer;
function GsDate_dailyValueFromMonthlyArray_NoLeap(var monthMeanArray: arrayMonths; date: GsDate): single;
function GsDate_dayOfYearFromDate_NoLeap(date: GsDate): integer;

{ output string functions }
function GsDate_dateString(date: GsDate): string;
function GsDate_monthYearString(date: GsDate): string;
function GsDate_yearString(date: GsDate): string;
function GsDate_monthNumberString(date: GsDate): string;
function GsDate_monthShortString(date: GsDate): string;
function GsDate_monthLongString(date: GsDate): string;
function GsDate_dayOfMonthString(date: GsDate): string;
function GsDate_dayOfWeekShortString(date: GsDate): string;
function GsDate_dayOfWeekLongString(date: GsDate): string;
function GsDate_MonthNameShort(month: integer): string;
function GsDate_MonthNameLong(month: integer): string;
function GsDate_DayOfWeekNameShort(dayOfWeek: integer): string;
function GsDate_DayOfWeekNameLong(dayOfWeek: integer): string;

{special calculations}
function GsDate_dateForFirstDayOfMonth(date: GsDate): GsDate;
function GsDate_dateForLastDayOfMonth(date: GsDate): GsDate;

implementation

uses SysUtils, ueutils;

{ initialization }
function GsDate_currentDate: GsDate;
  begin
  result.dateTime := now;
  end;

function GsDate_dateFromYearMonthDayOfMonth(year, month, dayOfMonth: word): GsDate;
  begin
  result := GsDate_encodeDate(year, month, dayOfMonth);
  end;

{ special encoding/decoding incorporating our indexes starting at zero }
procedure GsDate_decodeDate(date: GsDate; var year, month, dayOfMonth: word);
  begin
  { should add range-checking here }
  decodeDate(date.dateTime, year, month, dayOfMonth);
  dec(month);
  dec(dayOfMonth);
  end;

function GsDate_encodeDate(year, month, dayOfMonth: integer): GsDate;
  begin
  { should add range-checking here }
  result.dateTime := encodeDate(year, month + 1, dayOfMonth + 1);
  end;

{ incrementing }
procedure GsDate_addYears(var date: GsDate; changeInYears: integer);
  var
    year, month, dayOfMonth: word;
  begin
  GsDate_decodeDate(date, year, month, dayOfMonth);
  year := year + changeInYears;
  if GsDate_isLeapYearFromDate(date) and (month = kFebruary) and (dayOfMonth = 29 - 1)
    and not GsDate_isLeapYearFromDate(GsDate_encodeDate(year, month, 0)) then
    dec(dayOfMonth);
  date := GsDate_encodeDate(year, month, dayOfMonth);
  end;

procedure GsDate_addMonths(var date: GsDate; changeInMonths: integer);
  var
    year, dayOfMonth, monthWord: word;
    month: integer;
    yearsToAdd, monthsToAdd, daysInMonth: integer;
    temp: string;
  begin
  temp := GsDate_dateString(date);
  GsDate_decodeDate(date, year, monthWord, dayOfMonth);
  yearsToAdd := changeInMonths div 12;
  monthsToAdd := changeInMonths mod 12;
  year := year + yearsToAdd;
  month := monthWord;
  month := month + monthsToAdd;
  if month < kJanuary then
    begin
    month := month + 12;
    dec(year);
    end
  else if month > kDecember then
    begin
    month := month - 12;
    inc(year);
    end;
  monthWord := month;
  daysInMonth := GsDate_daysInMonthFromDate(GsDate_encodeDate(year, monthWord, 0));
  if dayOfMonth > daysInMonth - 1 then dayOfMonth := daysInMonth - 1;
  date := GsDate_encodeDate(year, monthWord, dayOfMonth);
  temp := GsDate_dateString(date);
  end;

procedure GsDate_addDays(var date: GsDate; changeInDays: integer);
  begin
  date.dateTime := date.dateTime + changeInDays;
  end;

{ date math using GsDates }
function GsDate_daysBetween(firstDate, secondDate: GsDate): longint;
  begin
  result := trunc(secondDate.dateTime) - trunc(firstDate.dateTime);
  end;

function GsDate_daysInXMonthsForwardFromDate(date: GsDate; months: integer): integer;
  var
    newDate: GsDate;
  begin
  newDate := date;
  GsDate_addMonths(newDate, months);
  result := GsDate_daysBetween(date, newDate);
  end;

function GsDate_firstDateIsEarlierThanSecond(firstDate, secondDate: GsDate): boolean;
  begin
  result := (trunc(firstDate.dateTime) < trunc(secondDate.dateTime));
  end;

function GsDate_firstDateIsLaterThanSecond(firstDate, secondDate: GsDate): boolean;
  begin
  result := (trunc(firstDate.dateTime) > trunc(secondDate.dateTime));
  end;

function GsDate_firstDateIsEarlierThanOrEqualToSecond(firstDate, secondDate: GsDate): boolean;
  begin
  result := (trunc(firstDate.dateTime) <= trunc(secondDate.dateTime));
  end;

function GsDate_dayOfYearFromDate(date: GsDate): integer;
  var
    year, month, dayOfMonth: word;
    firstOfYear: GsDate;
  begin
  GsDate_decodeDate(date, year, month, dayOfMonth);
  firstOfYear := GsDate_encodeDate(year, 0, 0);
  GsDate_addDays(firstOfYear, -1); { to get dec 31 of last year }
  result := GsDate_daysBetween(firstOfYear, date);
  end;

function GsDate_weekOfYearFromDate(date: GsDate): integer;
  begin
  { don't adjust for leap year here }
  result := GsDate_dayOfYearFromDate(date) div 7;
  end;

function GsDate_yearFromDate(date: GsDate): integer;
  var year, month, dayOfMonth: word;
  begin
  GsDate_decodeDate(date, year, month, dayOfMonth);
  result := year;
  end;

{zero based}
function GsDate_monthFromDate(date: GsDate): integer;
  var year, month, dayOfMonth: word;
  begin
  GsDate_decodeDate(date, year, month, dayOfMonth);
  result := month;
  end;

{zero based}
function GsDate_dayOfMonthFromDate(date: GsDate): integer;
  var year, month, dayOfMonth: word;
  begin
  GsDate_decodeDate(date, year, month, dayOfMonth);
  result := dayOfMonth;
  end;

function GsDate_isLeapYearFromDate(date: GsDate): boolean;
  var year, month, dayOfMonth: word;
  begin
  GsDate_decodeDate(date, year, month, dayOfMonth);
  result := (year mod 4 = 0) and ((year mod 100 <> 0) or (year mod 400 = 0));
  end;

function GsDate_daysInMonthFromDate(date: GsDate): integer;
  var
    year, month, dayOfMonth: word;
  begin
  GsDate_decodeDate(date, year, month, dayOfMonth);
  if (month = kJanuary) then
    result := cumDaysInMonthNonLeapYear[month]
  else if (month = kFebruary) and (GsDate_isLeapYearFromDate(date)) then
    result := (cumDaysInMonthNonLeapYear[month] + 1) - cumDaysInMonthNonLeapYear[month - 1] 
  else
    result := cumDaysInMonthNonLeapYear[month] - cumDaysInMonthNonLeapYear[month - 1];
  end;

{ date math not using GsDates - for these, don't care if is leap year }
function GsDate_monthFromDayOfYear_NoLeap(dayOfYear: integer): integer;
  var
    year, month, dayOfMonth: word;
    date: GsDate;
  begin
  date := GsDate_encodeDate(1901, 0, 0);
  GsDate_addDays(date, dayOfYear);
  GsDate_decodeDate(date, year, month, dayOfMonth);
  result := month;
  end;
(*  was:
  var month: integer;
  begin
  for month := kJanuary to kDecember do
    if dayOfYear <= cumDaysInMonthNonLeapYear[month] then
      begin
      result := month;
      exit;
      end;
  result := kDecember;
  end; *)

function GsDate_daysInMonthFromMonth_NoLeap(month: integer): integer;
  begin
  if (month = kJanuary) then
    result := cumDaysInMonthNonLeapYear[month]
  else
    result := cumDaysInMonthNonLeapYear[month] - cumDaysInMonthNonLeapYear[month - 1];
  end;

function GsDate_dayOfYearFromDayOfMonthAndMonth_NoLeap(dayOfMonth, month: integer): integer;
  var date: GsDate;
  begin
  date := GsDate_encodeDate(1901, month, dayOfMonth); {1900 is a leap year}
  result := GsDate_dayOfYearFromDate(date);
  end;

function GsDate_dayOfMonthFromDayOfYear_NoLeap(dayOfYear: integer): integer;
  var
    year, month, dayOfMonth: word;
    date: GsDate;
  begin
  date := GsDate_encodeDate(1901, 0, 0);
  GsDate_addDays(date, dayOfYear);
  GsDate_decodeDate(date, year, month, dayOfMonth);
  result := dayOfMonth;
  end;

function GsDate_dayOfYearFromDate_NoLeap(date: GsDate): integer;
  var
    year, month, dayOfMonth: word;
  begin
  GsDate_decodeDate(date, year, month, dayOfMonth);
  {28 is leap day feb 29 in zero based days}
  if (month = kFebruary) and (dayOfMonth = 28) then
    dayOfMonth := 28;
  result := GsDate_dayOfYearFromDayOfMonthAndMonth_NoLeap(dayOfMonth, month);
  end;

{ Note I used my function for this, not EPIC's. }
{ The only difference is that EPIC's function estimates based on the beginning of each month }
{ (January's mean temp takes place on January 1, for example) }
{ and this function estimates based on the 15th of each month }
{ (January's mean temp takes place on January 15, for example). }
{ I prefer this one because it places the mean on the mean day of the month. }
{ don't care about leap year here }
function GsDate_dailyValueFromMonthlyArray_NoLeap(var monthMeanArray: arrayMonths; date: GsDate): single;
  var
    fractionOfMonth, startMonthMean, endMonthMean: single;
    monthMeanDayThisMonth, startMonthMeanDay, endMonthMeanDay: integer;
    dayOfYear, month, monthToUse: integer;
  begin
  try
  dayOfYear := GsDate_dayOfYearFromDate(date);
  month := GsDate_monthFromDate(date);
  monthMeanDayThisMonth := GsDate_dayOfYearFromDayOfMonthAndMonth_NoLeap(15, month);
  if dayOfYear = monthMeanDayThisMonth then
    begin
    result := monthMeanArray[month];
    exit;
    end;
  if dayOfYear < monthMeanDayThisMonth then
    begin
    monthToUse := month - 1;
    if monthToUse < kJanuary then monthToUse := kDecember;
    startMonthMean := monthMeanArray[monthToUse];
    startMonthMeanDay := GsDate_dayOfYearFromDayOfMonthAndMonth_NoLeap(15, monthToUse);
    if monthToUse = kDecember then startMonthMeanDay := startMonthMeanDay - 365;
    endMonthMean := monthMeanArray[month];
    endMonthMeanDay := monthMeanDayThisMonth;
    end
  else 
    begin
    monthToUse := month + 1;
    if monthToUse > kDecember then monthToUse := kJanuary;
    startMonthMean := monthMeanArray[month];
    startMonthMeanDay := monthMeanDayThisMonth;
    endMonthMean := monthMeanArray[monthToUse];
    endMonthMeanDay := GsDate_dayOfYearFromDayOfMonthAndMonth_NoLeap(15, monthToUse);
    if monthToUse = kJanuary then endMonthMeanDay := endMonthMeanDay + 365;
    end;
  fractionOfMonth := safediv(1.0 * (dayOfYear - startMonthMeanDay), 1.0 * (endMonthMeanDay - startMonthMeanDay));
  result := startMonthMean + fractionOfMonth * (endMonthMean - startMonthMean);
  except
    result := errorMessage('Problem in GsDate_dailyValueFromMonthlyArray_NoLeap');
  end;
  end;

{ output string functions }
function GsDate_dateString(date: GsDate): string;
  begin
  result := formatDateTime('m/d/yyyy', date.dateTime);
  end;

function GsDate_monthYearString(date: GsDate): string;
  begin
  result := formatDateTime('m / yyyy', date.dateTime);
  end;

function GsDate_yearString(date: GsDate): string;
  var year, month, dayOfMonth: word;
  begin
  GsDate_decodeDate(date, year, month, dayOfMonth);
  result := intToStr(year);
  end;

function GsDate_monthNumberString(date: GsDate): string;
  var year, month, dayOfMonth: word;
  begin
  GsDate_decodeDate(date, year, month, dayOfMonth);
  result := intToStr(month + 1);
  end;

function GsDate_monthShortString(date: GsDate): string;
  var year, month, dayOfMonth: word;
  begin
  GsDate_decodeDate(date, year, month, dayOfMonth);
  result := GsDate_MonthNameShort(month + 1);
  end;

function GsDate_monthLongString(date: GsDate): string;
  var year, month, dayOfMonth: word;
  begin
  GsDate_decodeDate(date, year, month, dayOfMonth);
  result := GsDate_MonthNameLong(month + 1);
  end;

function GsDate_dayOfMonthString(date: GsDate): string;
  var year, month, dayOfMonth: word;
  begin
  GsDate_decodeDate(date, year, month, dayOfMonth);
  result := intToStr(dayOfMonth + 1);
  end;

function GsDate_dayOfWeekShortString(date: GsDate): string;
  begin
  result := GsDate_DayOfWeekNameShort(dayOfWeek(date.dateTime));
  end;

function GsDate_dayOfWeekLongString(date: GsDate): string;
  begin
  result := GsDate_DayOfWeekNameLong(dayOfWeek(date.dateTime));
  end;

{ functions to get strings from global arrays - to protect against out-of-range errors }
function GsDate_MonthNameShort(month: integer): string;
  begin
  if (month < 1) or (month > 12) then
    result := 'none'
  else
    result := ShortMonthNames[month];
  end;

function GsDate_MonthNameLong(month: integer): string;
  begin
  if (month < 1) or (month > 12) then
    result := 'none'
  else
    result := LongMonthNames[month];
  end;

function GsDate_DayOfWeekNameShort(dayOfWeek: integer): string;
  begin
  if (dayOfWeek < 1) or (dayOfWeek > 7) then
    result := 'none'
  else
    result := ShortDayNames[dayOfWeek];
  end;

function GsDate_DayOfWeekNameLong(dayOfWeek: integer): string;
  begin
  if (dayOfWeek < 1) or (dayOfWeek > 7) then
    result := 'none'
  else
    result := LongDayNames[dayOfWeek];
  end;

{special calculations}
function GsDate_dateForFirstDayOfMonth(date: GsDate): GsDate;
  var
    year, month, dayOfMonth: word;
  begin
  GsDate_decodeDate(date, year, month, dayOfMonth);
  dayOfMonth := 0;
  result := GsDate_encodeDate(year, month, dayOfMonth);
  end;

function GsDate_dateForLastDayOfMonth(date: GsDate): GsDate;
  var
    year, month, dayOfMonth: word;
  begin
  GsDate_decodeDate(date, year, month, dayOfMonth);
  dayOfMonth := GsDate_daysInMonthFromDate(date);
  result := GsDate_encodeDate(year, month, dayOfMonth);
  end;

begin
(*  GLOBAL DATE CONSTANTS
DateSeparator: Char;
ShortDateFormat: string[15];
LongDateFormat: string[31];
ShortMonthNames: array[1..12] of string[3];
LongMonthNames: array[1..12] of string[15];
ShortDayNames: array[1..7] of string[3];
LongDayNames: array[1..7] of string[15];
*)

end.
