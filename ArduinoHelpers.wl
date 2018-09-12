(* ::Package:: *)

BeginPackage["ArduinoHelpers`"];
DefaultSerialPort::usage="DefaultSerialPort is default serial port used by Arduino.";
OpenArduinoSerial::usage="OpenArduinoSerial[serial] opens serial port connected to Arduino device. By default DefaultSerialPort is used.";
CloseArduinoSerial::usage="CloseArduinoSerial[dev] closes Arduino serial connection.";
SynchronizeTime::usage="SynchronizeTime[dev] synchronizes Arduino against current Unix time.";
ReadTemperatureTrend::usage="ReadTemperatureTrend[dev] returns temperature as TimeSeries from given Arduino board.";
ReadTemperatureNow::usage="ReadTemperatureNow[dev] returns current temperature.";

Begin["ArduinoHelpers`Private`"];


DefaultSerialPort="/dev/ttyACM0";
LineFeedCharCode=ToCharacterCode["\n"][[1]];


OpenArduinoSerial[serial_:DefaultSerialPort]:=Module[{arduinoDev},
arduinoDev=DeviceOpen["Serial",{serial,"BaudRate"->9600}];
Return[arduinoDev];
];


CloseArduinoSerial[dev_]:=DeviceClose[dev];


SynchronizeTime::resp="Response timestamp `1` not expected `2`";

SynchronizeTime[dev_]:=Module[{currentTime=UnixTime[DateObject[TimeZone->0]],serialStr,responseStr,responseTimeStr,resp},
DeviceReadBuffer[dev, "ReadTerminator"->LineFeedCharCode];
serialStr=StringTemplate["T`unixtime`"][<|"unixtime"->currentTime|>];
DeviceWriteBuffer[dev,serialStr];
serialStr=FromCharacterCode[DeviceReadBuffer[dev,"ReadTerminator"->LineFeedCharCode]];
{responseStr,responseTimeStr}=StringCases[serialStr, {RegularExpression["Time synchronized. Current time is: "],RegularExpression["(?s)\\s*\\d+-\\d+-\\d+T\\d+:\\d+:\\d+\\.\\d+Z.*"]}];
resp=FromUnixTime[currentTime,TimeZone->0]==DateObject[responseTimeStr,TimeZone->0];
If[resp==False,Message[SynchronizeTime::resp, DateObject[responseTimeStr,TimeZone->0],FromUnixTime[currentTime,TimeZone->0]]];
Return[resp];
];


ReadTemperatureHistoryString[dev_]:=Module[{tempHistoryString},
DeviceReadBuffer[dev, "ReadTerminator"->LineFeedCharCode];
DeviceWriteBuffer[dev,"H"];
tempHistoryString=FromCharacterCode[DeviceReadBuffer[dev,"ReadTerminator"-> 23]];
Return[tempHistoryString];
];


ReadTemperatureTrend[dev_]:=Module[{regExpForNumbers,tempString,tempValues, timestamps, temperatureTrend, filteredTemperatureTrend},
regExpForNumbers=RegularExpression["[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)"];
tempString=ReadTemperatureHistoryString[dev];
{tempValues,timestamps}=StringCases[StringSplit[tempString, StartOfLine], regExpForNumbers]//ToExpression;
temperatureTrend={FromUnixTime/@timestamps, QuantityArray[tempValues,"Celcius"]}//Transpose;
filteredTemperatureTrend=DeleteCases[temperatureTrend,{FromUnixTime[0],Quantity[0.,"DegreesCelsius"]}];(*Drop empty measurements.*)
Return[filteredTemperatureTrend//EventSeries];
];


ReadTemperatureNow[dev_]:=Module[{tempString,timestamp,temperature},
DeviceReadBuffer[dev, "ReadTerminator"->LineFeedCharCode];
DeviceWriteBuffer[dev,"C"];
tempString=FromCharacterCode[DeviceReadBuffer[dev,"ReadTerminator"-> LineFeedCharCode]];
{timestamp, temperature} = 
 StringCases[
  tempString, {RegularExpression["\\s*\\d+-\\d+-\\d+T\\d+:\\d+:\\d+\\.\\d+Z"], 
   RegularExpression["[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)"]}];
Return[{DateObject[timestamp,TimeZone->0], Quantity[temperature//ToExpression,"DegreesCelsius"]}];
];


End[];


EndPackage[];
