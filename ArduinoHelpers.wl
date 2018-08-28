(* ::Package:: *)

BeginPackage["ArduinoHelpers`"];
DefaultSerialPort::usage="DefaultSerialPort is default serial port used by Arduino.";
OpenArduinoSerial::usage="OpenArduinoSerial[serial] opens serial port connected to Arduino device.";
CloseArduinoSerial::usage="CloseArduinoSerial[dev] closes Arduino serial connection.";
SynchronizeTime::usage="SynchronizeTime[dev] synchronizes Arduino against current Unix time.";
ReadTemperatureTrend::usage="ReadTemperatureTrend[dev] returns temperature as TimeSeries from given Arduino board.";

Begin["ArduinoHelpers`Private`"];


DefaultSerialPort="/dev/ttyACM0";
LineFeedCharCode=ToCharacterCode["\n"][[1]];


OpenArduinoSerial[serial_]:=Module[{arduinoDev},
arduinoDev=DeviceOpen["Serial",{serial,"BaudRate"->9600}];
Return[arduinoDev];
];


CloseArduinoSerial[dev_]:=DeviceClose[dev];


SynchronizeTime::resp="Response timestamp `1` not expected `2`";

SynchronizeTime[dev_]:=Module[{currentTime=UnixTime[DateObject[TimeZone->0]],timeSyncResponse,responseStr,responseTimeStr,resp},
DeviceWriteBuffer[dev,StringTemplate["T`unixtime`"][<|"unixtime"->currentTime|>]]
Pause[1];
timeSyncResponse=FromCharacterCode[DeviceReadBuffer[dev,"ReadTerminator"->LineFeedCharCode]];
{responseStr,responseTimeStr}=StringCases[timeSyncResponse, {RegularExpression["Time synchronized. Current time is: "],RegularExpression["(?s)\\s*\\d+-\\d+-\\d+T\\d+:\\d+:\\d+\\.\\d+Z.*"]}];
resp=FromUnixTime[currentTime,TimeZone->0]==DateObject[responseTimeStr,TimeZone->0];
If[resp==False,Message[SynchronizeTime::resp, DateObject[responseTimeStr,TimeZone->0],FromUnixTime[currentTime,TimeZone->0]]];
Return[resp];
];


ReadTemperatureHistoryString[dev_]:=Module[{tempHistoryString},
DeviceWriteBuffer[dev,"H"];
Pause[1];
tempHistoryString=FromCharacterCode[DeviceReadBuffer[dev,"ReadTerminator"-> 23]];
Return[tempHistoryString];
];


ReadTemperatureTrend[dev_]:=Module[{regExpForNumbers,tempString,tempValues, timestamps, temperatureTrend, filteredTemperatureTrend},
regExpForNumbers=RegularExpression["[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)"];
tempString=ReadTemperatureHistoryString[dev];
{tempValues,timestamps}=StringCases[StringSplit[tempString, StartOfLine], regExpForNumbers]//ToExpression;
temperatureTrend={FromUnixTime/@timestamps, tempValues}\[Transpose];
filteredTemperatureTrend=DeleteCases[temperatureTrend,{FromUnixTime[0],0.}];
Return[filteredTemperatureTrend//TimeSeries];
];


End[];


EndPackage[];
