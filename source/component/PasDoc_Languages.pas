{ @abstract(PasDoc language definitions and translations.)
  @author(Johannes Berg <johannes AT sipsolutions.de>)
  @author(Ralf Junker <delphi AT zeitungsjunge.de>)
  @author(Alexander Lisnevsky <alisnevsky AT yandex.ru> (Russian translation))
  @author(Hendy Irawan <ceefour AT gauldong.net> (Indonesian and Javanese translation))
  @author(Ivan Montes Velencoso (Catalan and Spanish translations))
  @author(Javi (Spanish translation))
  @author(Jean Dit Bailleul (Frensh translation))
  @author(Marc Weustinks (Dutch translation))
  @author(Martin Hansen <mh AT geus.dk> (Danish translation))
  @author(Michele Bersini <michele.bersini AT smartit.it> (Italian translation))
  @author(Peter Šimkoviè <simkovic_jr AT manal.sk> (Slovak translation))
  @author(Peter Thörnqvist <pt AT timemetrics.se> (Swedish translation))
  @author(Rodrigo Urubatan Ferreira Jardim <rodrigo AT netscape.net> (Brasilian translation))
  @author(Alexandre da Silva <simpsomboy AT gmail.com> (Brasilian translation - Update))
  @author(Vitaly Kovalenko <v_l_kovalenko AT alsy.by> (Russian translation))
  @author(Grzegorz Skoczylas <gskoczylas AT rekord.pl> (corrected Polish translation))
  @author(Jónás Gergõ <jonas.gergo AT ch...> (Hungarian translation))
  @author(Michalis Kamburelis)
  @author(Ascanio Pressato (Some Italian translation))
  @author(JBarbero Quiter (updated Spanish translation))
  @author(Liu Chuanjun <1000copy AT gmail.com> (Chinese gb2312 translation))
}

{-$DEFINE debug} //-as required

unit PasDoc_Languages;

interface

type
  { An enumeration type of all supported languages.
    Warning: order and count changed!
    This change may invalidate stored PasDoc_gui project settings?
  }
  TLanguageID = (
    lgDefault,
    lgBosnian,
    lgBrasilian,
    lgCatalan,
    lgChinese_950,
    lgChinese_gb2312,
    lgDanish,
    lgDutch,
    lgEnglish,
    lgFrench,
    lgGerman,
    lgIndonesian,
    lgItalian,
    lgJavanese,
    lgPolish_CP1250,
    lgPolish_ISO_8859_2,
    lgRussian_1251,
    lgRussian_866,
    lgRussian_koi8,
    lgSlovak,
    lgSpanish,
    lgSwedish,
    lgHungarian_1250
   );

  { An enumeration type of all static output texts.
    Warning: count and order changed!
  }
  TTranslationID = (
  //no translation ID assigned, so far
    trNoTrans,
  //map
    trUnits,
    trClassHierarchy,
    trCio,
    trIdentifiers,
    trGvUses,
    trGvClasses,
  //tables and members
    trClasses,
      trClass,
      trDispInterface,
      trInterface,
    trObjects,
      trObject,
      trRecord,
        trHierarchy,
        trFields,
        trMethods,
        trProperties,
    trLibrary,
    trPackage,
    trProgram,
    trUnit,
      trUses,
      trConstants,
      trFunctionsAndProcedures,
      trTypes,
        trType,
      trVariables,
      trAuthors,
        trAuthor,
      trCreated,
      trLastModified,
    trSubroutine,
      trParameters,
      trReturns,
      trExceptionsRaised,
    trExceptions,
      trException,
    trEnum,

  //visibilities
    trVisibility,
      trPrivate,
      trStrictPrivate,
      trProtected,
      trStrictProtected,
      trPublic,
      trPublished,
      trAutomated,
      trImplicit,
  //hints
    trDeprecated,
    trPlatformSpecific,
    trLibrarySpecific,

  //headings
    trOverview,
    trIntroduction,
    trConclusion,
    trHeadlineCio,
    trHeadlineConstants,
    trHeadlineFunctionsAndProcedures,
    trHeadlineIdentifiers,
    trHeadlineTypes,
    trHeadlineUnits,
    trHeadlineVariables,
    trSummaryCio,
  //column headings
    trDeclaration,
    trDescription,
    trName,
    trValues,

  //empty
    trNone,
    trNoCIOs,
    trNoCIOsForHierarchy,
    trNoTypes,
    trNoVariables,
    trNoConstants,
    trNoFunctions,
    trNoIdentifiers,

  //misc
    trHelp,
    trLegend,
    trMarker,

    trWarningOverwrite,
    trWarning,

    trGeneratedBy,
    trOnDateTime,

    trSearch,
    trSeeAlso,
  //add more here
    trDummy
  );

//const DEFAULT_LANGUAGE = lgEnglish;

type
//array holding the translated strings, or empty for default (English) text.
  RTransTable = array[TTranslationID] of string;
  PTransTable = ^RTransTable;

  { Language class to hold all translated strings.
    Warning: A new translation model is introduced now.
    All translations should be revised accordingly.
  }
  TPasDocLanguages = class
  private
    FLanguage: TLanguageID;
    procedure SetLanguage(const Value: TLanguageID);
  protected
    FCharSet: string;
  {$IFDEF old}
    FTranslation: array[TTranslationID] of string;
    { Defines translations for English (the default). }
    procedure SetLanguageEnglish;
    { Defines translations for German }
    procedure SetLanguageGerman;
  {$ELSE}
    pTable: PTransTable;
    { @abstract(gets a translation token) }
    function  GetTranslation(ATranslationID: TTranslationID): string;
    procedure SetTranslation(id: TTranslationID; const into: string);
    property FTranslation[id: TTranslationID]: string
      read GetTranslation write SetTranslation;
  {$ENDIF}
    { Defines translations for Bosnian. }
    procedure SetLanguageBosnian;
    { Defines translations for Brasilian. }
    procedure SetLanguageBrasilian;
    { Defines translations for Catalan. }
    procedure SetLanguageCatalan;
    { Defines translations for Chinese (Codepage 950). }
    procedure SetLanguageChinese_950;
    procedure SetLanguageChinese_gb2312;
    { Defines translations for Danish. }
    procedure SetLanguageDanish;
    { Defines translations for Dutch. }
    procedure SetLanguageDutch;
    { Defines translations for French. }
    procedure SetLanguageFrench;
    { Defines translations for Indonesian. }
    procedure SetLanguageIndonesian;
    { Defines translations for Italian. }
    procedure SetLanguageItalian;
    { Defines translations for Javanese. }
    procedure SetLanguageJavanese;
    { Defines translations for Polish (Codepage 1250). }
    procedure SetLanguagePolish_CP1250;
    { Defines translations for Polish (Codepage ISO 8859-2). }
    procedure SetLanguagePolish_ISO_8859_2;
    { Defines translations for Russian (Codepage 1251). }
    procedure SetLanguageRussian_1251;
    { Defines translations for Russian (Codepage 866). }
    procedure SetLanguageRussian_866;
    { Defines translations for Russian (KOI-8). }
    procedure SetLanguageRussian_koi8;
    { Defines translations for Slovak. }
    procedure SetLanguageSlovak;
    { Defines translations for Spanish. }
    procedure SetLanguageSpanish;
    { Defines translations for Swedish. }
    procedure SetLanguageSwedish;
    { Defines translations for Hungarian (Codepage 1250). }
    procedure SetLanguageHungarian_1250;


  public
    { Charset for current language }
    property CharSet: string read FCharSet;
    property Translation[ATranslationID: TTranslationID]: string read GetTranslation;
    constructor Create;
    property Language: TLanguageID read FLanguage write SetLanguage
      default lgDefault;
  end;

//Some GUI helpers
{}

//Full language name
function LanguageFromIndex(i: integer): string;

//Language abbreviation
function SyntaxFromIndex(i: integer): string;

//Search for language by short or long name
function IDfromLanguage(const s: string): TLanguageID;

//Manual translation of id into lang
function Translation(id: TTranslationID; lang: TLanguageID): string;

{$IFDEF debug}
function TranslationNameFromId(id: TTranslationID): string;
{$ELSE}
{$ENDIF}

{ Find a language with Syntax = S (case ignored).
  Returns @true and sets LanguageId if found, otherwise returns @false. }
function LanguageFromStr(S: string; out LanguageId: TLanguageID): boolean;

implementation

{$IFDEF fpc}
{$ELSE}
//Delphi
uses
  SysUtils;
{$ENDIF}

{$IFDEF debug}
const
//for debugging purposes: the dump should read like the dump of aEnglish.
  aTransIdNames: array[TTranslationID] of string = (
  //no translation ID assigned, so far
    'trNoTrans',
  //map
    'trUnits',
    'trClassHierarchy',
    'trCio',
    'trIdentifiers',
    'trGvUses',
    'trGvClasses',
  //tables and members
    'trClasses',
      'trClass',
      'trDispInterface',
      'trInterface',
    'trObjects',
      'trObject',
      'trRecord',
        'trHierarchy',
        'trFields',
        'trMethods',
        'trProperties',
    'trLibrary',
    'trPackage',
    'trProgram',
    'trUnit',
      'trUses',
      'trConstants',
      'trFunctionsAndProcedures',
      'trTypes',
        'trType',
      'trVariables',
      'trAuthors',
        'trAuthor',
      'trCreated',
      'trLastModified',
    'trSubroutine',
      'trParameters',
      'trReturns',
      'trExceptionsRaised',
    'trExceptions',
      'trException',
    'trEnum',

  //visibilities
    'trVisibility',
      'trPrivate',
      'trStrictPrivate',
      'trProtected',
      'trStrictProtected',
      'trPublic',
      'trPublished',
      'trAutomated',
      'trImplicit',
  //hints
    'trDeprecated',
    'trPlatformSpecific',
    'trLibrarySpecific',

  //headings
    'trOverview',
    'trIntroduction',
    'trConclusion',
    'trHeadlineCio',
    'trHeadlineConstants',
    'trHeadlineFunctionsAndProcedures',
    'trHeadlineIdentifiers',
    'trHeadlineTypes',
    'trHeadlineUnits',
    'trHeadlineVariables',
    'trSummaryCio',
  //column headings
    'trDeclaration',
    'trDescription',
    'trName',
    'trValues',

  //empty
    'trNone',
    'trNoCIOs',
    'trNoCIOsForHierarchy',
    'trNoTypes',
    'trNoVariables',
    'trNoConstants',
    'trNoFunctions',
    'trNoIdentifiers',

  //misc
    'trHelp',
    'trLegend',
    'trMarker',

    'trWarningOverwrite',
    'trWarning',

    'trGeneratedBy',
    'trOnDateTime',

    'trSearch',
    'trSeeAlso',
  //add more here
    'trDummy'
  );
{$ELSE}
{$ENDIF}

const
(* Translation markers
  For ease of finding missing translations, special markers can be used:
  strToDo should be obvious ;-)
  strKeep means to keep the English (default language) wording.
*)
{$IFDEF debug}
  strKeep = '='; //keep English wording
  strToDo = '?'; //to be translated
{$ELSE}
  strKeep = ''; //'='? keep English wording
  strToDo = ''; //'?'? to be translated
{$ENDIF}

(* Language template:
  Copy aNewLanguage into a const section and rename it to the new language name.
  Put a reference to the new const array into LANGUAGE_ARRAY[...].Table.
  Then replace all occurences of strToDo by your translation of the text in the comment,
  or rename them into strKeep for all strings that need no translation.
  Eventually delete the //... comments, to save file space.
*)
var //writeable, for old (explicit) setup
  aNewLanguage: RTransTable = (
    {trNoTrans} '<what?>', //not ID assigned, so far
  //map
    {trUnits} strToDo, //'Units',
    {trClassHierarchy} strToDo, //'Class Hierarchy',
    {trCio} strToDo, //'Classes, Interfaces, Objects and Records',
    {trIdentifiers} strToDo, //'Identifiers',
    {trGvUses} strToDo, //'Unit dependency graph',
    {trGvClasses} strToDo, //'Classes hierarchy graph',
  //tables and members
    {trClasses} strToDo, //'Classes',
      {trClass} strToDo, //'Class',
      {trDispInterface} strToDo, //'DispInterface',
      {trInterface} strToDo, //'Interface'
    {trObjects} strToDo, //'Objects',
      {trObject} strToDo, //'Object',
      {trRecord} strToDo, //'Record',
        {trHierarchy} strToDo, //'Hierarchy',
        {trFields} strToDo, //'Fields',
        {trMethods} strToDo, //'Methods',
        {trProperties} strToDo, //'Properties',
    {trLibrary} strToDo,  //'Library',
    {trPackage} strToDo,  //'Package',
    {trProgram} strToDo,  //'Program',
    {trUnit} strToDo, //'Unit',
      {trUses} strToDo, //'Uses',
      {trConstants} strToDo, //'Constants',
      {trFunctionsAndProcedures} strToDo, //'Functions and Procedures',
      {trTypes} strToDo, //'Types',
        {trType} strToDo, //'Type',
      {trVariables} strToDo, //'Variables',
      {trAuthors} strToDo, //'Authors',
        {trAuthor} strToDo, //'Author',
      {trCreated} strToDo, //'Created',
      {trLastModified} strToDo, //'Last Modified',
    {trSubroutine} strToDo, //'Subroutine',
      {trParameters} strToDo, //'Parameters',
      {trReturns} strToDo, //'Returns',
      {trExceptionsRaised} strToDo, //'Exceptions raised',
    {trExceptions} strToDo, //'Exceptions',
      {trException} strToDo, //'Exception',
    {trEnum} strToDo, //'Enumeration',
  //visibilities
    {trVisibility} strToDo, //'Visibility',
      {trPrivate} strToDo, //'Private',
      {trStrictPrivate} strToDo, //'Strict Private',
      {trProtected} strToDo, //'Protected',
      {trStrictProtected} strToDo, //'Strict Protected',
      {trPublic} strToDo, //'Public',
      {trPublished} strToDo, //'Published',
      {trAutomated} strToDo, //'Automated',
      {trImplicit} strToDo, //'Implicit',
  //hints
    {trDeprecated} strToDo, //'this symbol is deprecated',
    {trPlatformSpecific} strToDo, //'this symbol is specific to some platform',
    {trLibrarySpecific} strToDo, //'this symbol is specific to some library',
  //headings
    {trOverview} strToDo, //'Overview',
    {trIntroduction} strToDo, //'Introduction',
    {trConclusion} strToDo, //'Conclusion',
    {trHeadlineCio} strToDo, //'All Classes, Interfaces, Objects and Records',
    {trHeadlineConstants} strToDo, //'All Constants',
    {trHeadlineFunctionsAndProcedures} strToDo, //'All Functions and Procedures',
    {trHeadlineIdentifiers} strToDo, //'All Identifiers',
    {trHeadlineTypes} strToDo, //'All Types',
    {trHeadlineUnits} strToDo, //'All Units',
    {trHeadlineVariables} strToDo, //'All Variables',
    {trSummaryCio} strToDo, //'Summary of Classes, Interfaces, Objects and Records',
  //column headings
    {trDeclaration} strToDo, //'Declaration',
    {trDescription} strToDo, //'Description',
    {trName} strToDo, //'Name',
    {trValues} strToDo, //'Values',
  //empty
    {trNone} strToDo, //'None',
    {trNoCIOs} strToDo, //'The units do not contain any classes, interfaces, objects or records.',
    {trNoCIOsForHierarchy} strToDo, //'The units do not contain any classes, interfaces or objects.',
    {trNoTypes} strToDo, //'The units do not contain any types.',
    {trNoVariables} strToDo, //'The units do not contain any variables.',
    {trNoConstants} strToDo, //'The units do not contain any constants.',
    {trNoFunctions} strToDo, //'The units do not contain any functions or procedures.',
    {trNoIdentifiers} strToDo, //'The units do not contain any identifiers.',
  //misc
    {trHelp} strToDo, //'Help',
    {trLegend} strToDo, //'Legend',
    {trMarker} strToDo, //'Marker',
    {trWarningOverwrite} strToDo, //'Warning: Do not edit - this file has been created automatically and is likely be overwritten',
    {trWarning} strToDo, //'Warning',
    {trGeneratedBy} strToDo, //'Generated by',
    {trOnDateTime} strToDo, //'on',
    {trSearch} strToDo, //'Search',
    {trSeeAlso} strToDo, //'See also',
    ''  //dummy
  );

{$IFDEF old}

procedure TPasDocLanguages.SetLanguageEnglish;
begin
  FTranslation[trAuthor] := 'Author';
  FTranslation[trAuthors] := 'Authors';
  FTranslation[trAutomated] := 'Automated';
  FTranslation[trCio] := 'Classes, Interfaces, Objects and Records';
  FTranslation[trClass] := 'Class';
  FTranslation[trClasses] := 'Classes';
  FTranslation[trClassHierarchy] := 'Class Hierarchy';
  FTranslation[trConstants] := 'Constants';
  FTranslation[trCreated] := 'Created';
  FTranslation[trDeclaration] := 'Declaration';
  FTranslation[trDescription] := 'Description';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions';
  FTranslation[trExceptionsRaised] := 'Exceptions raised';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Fields';
  FTranslation[trFunctionsAndProcedures] := 'Functions and Procedures';
  FTranslation[trHelp] := 'Help';
  FTranslation[trHierarchy] := 'Hierarchy';
  FTranslation[trIdentifiers] := 'Identifiers';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLegend] := 'Legend';
  FTranslation[trMarker] := 'Marker';
  FTranslation[trVisibility] := 'Visibility';
  FTranslation[trMethods] := 'Methods';
  FTranslation[trLastModified] := 'Last Modified';
  FTranslation[trName] := 'Name';
  FTranslation[trNone] := 'None';
  FTranslation[trObject] := 'Object';
  FTranslation[trObjects] := 'Objects';
  FTranslation[trOverview] := 'Overview';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trStrictPrivate] := 'Strict Private';
  FTranslation[trProperties] := 'Properties';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trStrictProtected] := 'Strict Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Type';
  FTranslation[trTypes] := 'Types';
  FTranslation[trUnit] := 'Unit';
  FTranslation[trUnits] := 'Units';
  FTranslation[trVariables] := 'Variables';
  FTranslation[trGvUses] := 'Unit dependency graph';
  FTranslation[trGvClasses] := 'Classes hierarchy graph';
  FTranslation[trHeadlineCio] := 'All Classes, Interfaces, Objects and Records';
  FTranslation[trHeadlineConstants] := 'All Constants';
  FTranslation[trHeadlineFunctionsAndProcedures] := 'All Functions and Procedures';
  FTranslation[trHeadlineIdentifiers] := 'All Identifiers';
  FTranslation[trHeadlineTypes] := 'All Types';
  FTranslation[trHeadlineUnits] := 'All Units';
  FTranslation[trHeadlineVariables] := 'All Variables';
  FTranslation[trSummaryCio] := 'Summary of Classes, Interfaces, Objects and Records';
  FTranslation[trWarningOverwrite] :=
    'Warning: Do not edit - this file has been created automatically and is likely be overwritten';
  FTranslation[trWarning] := 'Warning';
  FTranslation[trGeneratedBy] := 'Generated by';
  FTranslation[trOnDateTime] := 'on';
  FTranslation[trDeprecated] := 'this symbol is deprecated';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library';
  FTranslation[trIntroduction] := 'Introduction';
  FTranslation[trConclusion] := 'Conclusion';
  FTranslation[trSearch] := 'Search';
  FTranslation[trSeeAlso] := 'See also';
  FTranslation[trValues] := 'Values';
  FTranslation[trNoCIOs] := 'The units do not contain any classes, interfaces, objects or records.';
  FTranslation[trNoCIOsForHierarchy] := 'The units do not contain any classes, interfaces or objects.';
  FTranslation[trNoTypes] := 'The units do not contain any types.';
  FTranslation[trNoVariables] := 'The units do not contain any variables.';
  FTranslation[trNoConstants] := 'The units do not contain any constants.';
  FTranslation[trNoFunctions] := 'The units do not contain any functions or procedures.';
  FTranslation[trNoIdentifiers] := 'The units do not contain any identifiers.';
  FTranslation[trProgram] := 'Program';
  FTranslation[trLibrary] := 'Library';
end;
{$ELSE}

const
  aEnglish: RTransTable = (
    {trNoTrans} '<what?>', //not ID assigned, so far
  //map
    {trUnits} 'Units',
    {trClassHierarchy} 'Class Hierarchy',
    {trCio} 'Classes, Interfaces, Objects and Records',
    {trIdentifiers} 'Identifiers',
    {trGvUses} 'Unit dependency graph',
    {trGvClasses} 'Classes hierarchy graph',
  //tables and members
    {trClasses} 'Classes',
      {trClass} 'Class',
      {trDispInterface} 'DispInterface',
      {trInterface} 'Interface',
    {trObjects} 'Objects',
      {trObject} 'Object',
      {trRecord} 'Record',
        {trHierarchy} 'Hierarchy',
        {trFields} 'Fields',
        {trMethods} 'Methods',
        {trProperties} 'Properties',
    {trLibrary} 'Library',
    {trPackage} 'Package',
    {trProgram} 'Program',
    {trUnit} 'Unit',
      {trUses} 'Uses',
      {trConstants} 'Constants',
      {trFunctionsAndProcedures} 'Functions and Procedures',
      {trTypes} 'Types',
        {trType} 'Type',
      {trVariables} 'Variables',
      {trAuthors} 'Authors',
        {trAuthor} 'Author',
      {trCreated} 'Created',
      {trLastModified} 'Last Modified',
    {trSubroutine} 'Subroutine',
      {trParameters} 'Parameters',
      {trReturns} 'Returns',
      {trExceptionsRaised} 'Exceptions raised',
    {trExceptions} 'Exceptions',
      {trException} 'Exception',
    {trEnum} 'Enumeration',
  //visibilities
    {trVisibility} 'Visibility',
      {trPrivate} 'Private',
      {trStrictPrivate} 'Strict Private',
      {trProtected} 'Protected',
      {trStrictProtected} 'Strict Protected',
      {trPublic} 'Public',
      {trPublished} 'Published',
      {trAutomated} 'Automated',
      {trImplicit} 'Implicit',
  //hints
    {trDeprecated} 'this symbol is deprecated',
    {trPlatformSpecific} 'this symbol is specific to some platform',
    {trLibrarySpecific} 'this symbol is specific to some library',
  //headings
    {trOverview} 'Overview',
    {trIntroduction} 'Introduction',
    {trConclusion} 'Conclusion',
    {trHeadlineCio} 'All Classes, Interfaces, Objects and Records',
    {trHeadlineConstants} 'All Constants',
    {trHeadlineFunctionsAndProcedures} 'All Functions and Procedures',
    {trHeadlineIdentifiers} 'All Identifiers',
    {trHeadlineTypes} 'All Types',
    {trHeadlineUnits} 'All Units',
    {trHeadlineVariables} 'All Variables',
    {trSummaryCio} 'Summary of Classes, Interfaces, Objects and Records',
  //column headings
    {trDeclaration} 'Declaration',
    {trDescription} 'Description',
    {trName} 'Name',
    {trValues} 'Values',
  //empty
    {trNone} 'None',
    {trNoCIOs} 'The units do not contain any classes, interfaces, objects or records.',
    {trNoCIOsForHierarchy} 'The units do not contain any classes, interfaces or objects.',
    {trNoTypes} 'The units do not contain any types.',
    {trNoVariables} 'The units do not contain any variables.',
    {trNoConstants} 'The units do not contain any constants.',
    {trNoFunctions} 'The units do not contain any functions or procedures.',
    {trNoIdentifiers} 'The units do not contain any identifiers.',
  //misc
    {trHelp} 'Help',
    {trLegend} 'Legend',
    {trMarker} 'Marker',
    {trWarningOverwrite} 'Warning: Do not edit - this file has been created automatically and is likely be overwritten',
    {trWarning} 'Warning',
    {trGeneratedBy} 'Generated by',
    {trOnDateTime} 'on',
    {trSearch} 'Search',
    {trSeeAlso} 'See also',
    ''  //dummy
  );
{$ENDIF}

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageBosnian;
begin
  FTranslation[trAuthor] := 'Autor';
  FTranslation[trAuthors] := 'Autori';
  FTranslation[trCio] := 'Klase, Interfejsi i Objekti';
  FTranslation[trClass] := 'Klasa';
  FTranslation[trClasses] := 'Klase';
  FTranslation[trClassHierarchy] := 'Klasna hijerarhija';
  FTranslation[trConstants] := 'Konstante';
  FTranslation[trCreated] := 'Kreirano';
  FTranslation[trDeclaration] := 'Deklaracija';
  FTranslation[trDescription] := 'Opis';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions'; // TODO: translate
  FTranslation[trExceptionsRaised] := 'Exceptions raised'; // TODO: translate
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Polja';
  FTranslation[trFunctionsAndProcedures] := 'Funkcije i Procedure';
  FTranslation[trHelp] := 'Pomoæ';
  FTranslation[trHierarchy] := 'Hijerarhija';
  FTranslation[trIdentifiers] := 'Identifikatori';
  FTranslation[trInterface] := 'Interfejs';
  FTranslation[trLegend] := 'Legenda';
  FTranslation[trMarker] := 'Marker'; // TODO: translate
  FTranslation[trVisibility] := 'Visibility'; // TODO: translate
  FTranslation[trMethods] := 'Metode';
  FTranslation[trLastModified] := 'Zadnja promjena';
  FTranslation[trName] := 'Ime';
  FTranslation[trNone] := 'Ništa';
  FTranslation[trObject] := 'Objekt';
  FTranslation[trObjects] := 'Objekti';
  FTranslation[trOverview] := 'Pregled';
  FTranslation[trPrivate] := 'Privatni';
  FTranslation[trProperties] := 'Osibine';
  FTranslation[trProtected] := 'Zaštiæen';
  FTranslation[trPublic] := 'Publikovan';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Javan';
  FTranslation[trType] := 'Tip';
  FTranslation[trTypes] := 'Tipovi';
  FTranslation[trUnit] := 'Fajl';
  FTranslation[trUnits] := 'Fajlovi';
  FTranslation[trVariables] := 'Promjenjive';
  FTranslation[trGvUses] := 'Unit dependency graph'; // TODO: translate
  FTranslation[trGvClasses] := 'Classes hierarchy graph'; // TODO: translate
  FTranslation[trHeadlineCio] := 'Sve Klase, Interfejsi i Objekti';
  FTranslation[trHeadlineConstants] := 'Sve Konstante';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Sve Funkcije i Procedure';
  FTranslation[trHeadlineIdentifiers] := 'Svi Identifikatoti';
  FTranslation[trHeadlineTypes] := 'Svi Tipovi';
  FTranslation[trHeadlineUnits] := 'Svi Fajlovi';
  FTranslation[trHeadlineVariables] := 'Sve Varijable';
  FTranslation[trSummaryCio] := 'Zbirno od Klasa, Interfejsa i Objekata';
  FTranslation[trWarningOverwrite] :=
    'Upozorenje: Ne mjenjajte fajl - ovaj fajl je kreiran automatski i velika je vjerovatnoæa da æe biti prepisan';
  FTranslation[trWarning] := 'Warning';  // TODO: translate
  FTranslation[trGeneratedBy] := 'Generated by'; // TODO: translate
  FTranslation[trOnDateTime] := 'on'; // TODO: translate
  FTranslation[trDeprecated] := 'this symbol is deprecated'; // TODO: translate
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform'; // TODO: translate
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library'; // TODO: translate
  FTranslation[trIntroduction] := 'Introduction'; // TODO: translate
  FTranslation[trConclusion] := 'Conclusion'; // TODO: translate
  FTranslation[trSearch] := 'Search'; // TODO: translate
  FTranslation[trSeeAlso] := 'See also'; // TODO: translate
  FTranslation[trValues] := 'Values'; // TODO: translate
  FTranslation[trNoCIOs] := 'The units do not contain any classes, interfaces, objects or records.'; // TODO: translate
  FTranslation[trNoCIOsForHierarchy] := 'The units do not contain any classes, interfaces or objects.'; // TODO: translate
  FTranslation[trNoTypes] := 'The units do not contain any types.'; // TODO: translate
  FTranslation[trNoVariables] := 'The units do not contain any variables.'; // TODO: translate
  FTranslation[trNoConstants] := 'The units do not contain any constants.'; // TODO: translate
  FTranslation[trNoFunctions] := 'The units do not contain any functions or procedures.'; // TODO: translate
  FTranslation[trNoIdentifiers] := 'The units do not contain any identifiers.'; // TODO: translate
  FTranslation[trProgram] := 'Program'; // TODO: translate
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageBrasilian;
begin
  FTranslation[trAuthor] := 'Autor';
  FTranslation[trAuthors] := 'Autores';
  FTranslation[trAutomated] := 'Automated';
  FTranslation[trCio] := 'Classes, Interfaces, Objetos e Registros';
  FTranslation[trClass] := 'Classe';
  FTranslation[trClasses] := 'Classes';
  FTranslation[trClassHierarchy] := 'Hierarquia de Classes';
  FTranslation[trConstants] := 'Constantes';
  FTranslation[trCreated] := 'Criada';
  FTranslation[trDeclaration] := 'Declaração';
  FTranslation[trDescription] := 'Descrição';
  FTranslation[trParameters] := 'Parâmetros';
  FTranslation[trReturns] := 'Retornos';
  FTranslation[trExceptions] := 'Exceções';
  FTranslation[trExceptionsRaised] := 'Exceções'; // TODO: translate as "Exceptions raised" instead of just "Exceptions"
  FTranslation[trEnum] := 'Enumerações';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Campos';
  FTranslation[trFunctionsAndProcedures] := 'Funções e Procedimentos';
  FTranslation[trHelp] := 'Ajuda';
  FTranslation[trHierarchy] := 'Hierarquia';
  FTranslation[trIdentifiers] := 'Identificadores';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLegend] := 'Legenda';
  FTranslation[trMarker] := 'Marker'; // TODO: translate
  FTranslation[trVisibility] := 'Visibility'; // TODO: translate
  FTranslation[trMethods] := 'Métodos';
  FTranslation[trLastModified] := 'Última modificação';
  FTranslation[trName] := 'Nome';
  FTranslation[trNone] := 'Nenhum';
  FTranslation[trObject] := 'Objeto';
  FTranslation[trObjects] := 'Objetos';
  FTranslation[trOverview] := 'Visão Geral';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Properties';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Tipo';
  FTranslation[trTypes] := 'Tipos';
  FTranslation[trUnit] := 'Unit';
  FTranslation[trUnits] := 'Units';
  FTranslation[trVariables] := 'Variáveis';
  FTranslation[trGvUses] := 'Diagrama de dependências de units';
  FTranslation[trGvClasses] := 'Diagrama de hierarquia de Classes';
  FTranslation[trHeadlineCio] := 'Todas as Classes, Interfaces, Objetos e Registros';
  FTranslation[trHeadlineConstants] := 'Todas as Constantes';
  FTranslation[trHeadlineFunctionsAndProcedures] := 'Todas as funções e procedimentos';
  FTranslation[trHeadlineIdentifiers] := 'Todos os Identificadores';
  FTranslation[trHeadlineTypes] := 'Todos os Tipos';
  FTranslation[trHeadlineUnits] := 'Todas as Units';
  FTranslation[trHeadlineVariables] := 'Todas as Variáveis';
  FTranslation[trSummaryCio] :=
    'Lista das Classes, Interfaces, Objetos e Registros';
  FTranslation[trWarningOverwrite] :=
    'Aviso, não altere - este arquivo foi gerado automaticamente e será sobrescrito';
  FTranslation[trWarning] := 'Warning';  // TODO: translate
  FTranslation[trGeneratedBy] := 'Gerado por';
  FTranslation[trOnDateTime] := 'as';
  FTranslation[trDeprecated] := 'este símbolo está depreciado';
  FTranslation[trPlatformSpecific] := 'este símbolo é específico para alguma plataforma';
  FTranslation[trLibrarySpecific] := 'este símbolo é específico para alguma biblioteca';
  FTranslation[trIntroduction] := 'Introduction'; // TODO: translate
  FTranslation[trConclusion] := 'Conclusion'; // TODO: translate
  FTranslation[trSearch] := 'Search'; // TODO: translate
  FTranslation[trSeeAlso] := 'See also'; // TODO: translate
  FTranslation[trValues] := 'Values'; // TODO: translate
  FTranslation[trNoCIOs] := 'The units do not contain any classes, interfaces, objects or records.'; // TODO: translate
  FTranslation[trNoCIOsForHierarchy] := 'The units do not contain any classes, interfaces or objects.'; // TODO: translate
  FTranslation[trNoTypes] := 'The units do not contain any types.'; // TODO: translate
  FTranslation[trNoVariables] := 'The units do not contain any variables.'; // TODO: translate
  FTranslation[trNoConstants] := 'The units do not contain any constants.'; // TODO: translate
  FTranslation[trNoFunctions] := 'The units do not contain any functions or procedures.'; // TODO: translate
  FTranslation[trNoIdentifiers] := 'The units do not contain any identifiers.'; // TODO: translate
  FTranslation[trProgram] := 'Program'; // TODO: translate
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageCatalan;
begin
  FTranslation[trAuthor] := 'Autor';
  FTranslation[trAuthors] := 'Autors';
  FTranslation[trCio] := 'Clases, interfaces i objectes';
  FTranslation[trClass] := 'Clase';
  FTranslation[trClasses] := 'Clases';
  FTranslation[trConstants] := 'Constants';
  FTranslation[trCreated] := 'Creat';
  FTranslation[trDeclaration] := 'Declaraci¢';
  FTranslation[trDescription] := 'Descripci¢';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions'; // TODO: translate
  FTranslation[trExceptionsRaised] := 'Exceptions raised'; // TODO: translate
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Camps';
  FTranslation[trFunctionsAndProcedures] := 'Funcions i procediments';
  FTranslation[trHelp] := 'Help';
  FTranslation[trHierarchy] := 'Hierarchy';
  FTranslation[trIdentifiers] := 'Identificadors';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLastModified] := 'Éltima modificaci¢';
  FTranslation[trLegend] := 'Legend';
  FTranslation[trMarker] := 'Marker'; // TODO: translate
  FTranslation[trVisibility] := 'Visibility'; // TODO: translate
  FTranslation[trMethods] := 'MŠtodes';
  FTranslation[trName] := 'Nom';
  FTranslation[trNone] := 'Ningu';
  FTranslation[trObject] := 'Objecte';
  FTranslation[trObjects] := 'Objectes';
  FTranslation[trOverview] := 'Resum';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Propietats';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Tipus';
  FTranslation[trTypes] := 'Tipus';
  FTranslation[trUnit] := 'Unitat';
  FTranslation[trUnits] := 'Unitats';
  FTranslation[trVariables] := 'Variables';
  FTranslation[trGvUses] := 'Unit dependency graph'; // TODO: translate
  FTranslation[trGvClasses] := 'Classes hierarchy graph'; // TODO: translate
  FTranslation[trWarningOverwrite] :=
    'Atenci¢, no editar - aquest fitxer ha estat creat automaticament i ser… sobrescrit';
  FTranslation[trWarning] := 'Warning';  // TODO: translate
  FTranslation[trHeadlineCio] := 'Totes les clases, interfaces i objectes';
  FTranslation[trHeadlineConstants] := 'Totes les constants';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Totes les funcions i procediments';
  FTranslation[trHeadlineIdentifiers] := 'Tot els indentificadors';
  FTranslation[trHeadlineTypes] := 'Tots els tipus';
  FTranslation[trHeadlineUnits] := 'Totes les unitats';
  FTranslation[trHeadlineVariables] := 'Totes les variables';
  FTranslation[trSummaryCio] := 'Llista de clases, interfaces i objectes';
  FTranslation[trGeneratedBy] := 'Generated by'; // TODO: translate
  FTranslation[trOnDateTime] := 'on'; // TODO: translate
  FTranslation[trDeprecated] := 'this symbol is deprecated'; // TODO: translate
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform'; // TODO: translate
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library'; // TODO: translate
  FTranslation[trIntroduction] := 'Introduction'; // TODO: translate
  FTranslation[trConclusion] := 'Conclusion'; // TODO: translate
  FTranslation[trSearch] := 'Search'; // TODO: translate
  FTranslation[trSeeAlso] := 'See also'; // TODO: translate
  FTranslation[trValues] := 'Values'; // TODO: translate
  FTranslation[trNoCIOs] := 'The units do not contain any classes, interfaces, objects or records.'; // TODO: translate
  FTranslation[trNoCIOsForHierarchy] := 'The units do not contain any classes, interfaces or objects.'; // TODO: translate
  FTranslation[trNoTypes] := 'The units do not contain any types.'; // TODO: translate
  FTranslation[trNoVariables] := 'The units do not contain any variables.'; // TODO: translate
  FTranslation[trNoConstants] := 'The units do not contain any constants.'; // TODO: translate
  FTranslation[trNoFunctions] := 'The units do not contain any functions or procedures.'; // TODO: translate
  FTranslation[trNoIdentifiers] := 'The units do not contain any identifiers.'; // TODO: translate
  FTranslation[trProgram] := 'Program'; // TODO: translate
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageChinese_950;
begin
  FTranslation[trAuthor] := '§@ªÌ';
  FTranslation[trAuthors] := '§@ªÌ¸s';
  FTranslation[trGeneratedBy] := 'Generated by'; // TODO: translate
  FTranslation[trOnDateTime] := 'on'; // TODO: translate
  FTranslation[trDeprecated] := 'this symbol is deprecated'; // TODO: translate
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform'; // TODO: translate
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library'; // TODO: translate
  FTranslation[trIntroduction] := 'Introduction'; // TODO: translate
  FTranslation[trConclusion] := 'Conclusion'; // TODO: translate
  FTranslation[trSearch] := 'Search'; // TODO: translate
  FTranslation[trSeeAlso] := 'See also'; // TODO: translate
  FTranslation[trValues] := 'Values'; // TODO: translate
  FTranslation[trNoCIOs] := 'The units do not contain any classes, interfaces, objects or records.'; // TODO: translate
  FTranslation[trNoCIOsForHierarchy] := 'The units do not contain any classes, interfaces or objects.'; // TODO: translate
  FTranslation[trNoTypes] := 'The units do not contain any types.'; // TODO: translate
  FTranslation[trNoVariables] := 'The units do not contain any variables.'; // TODO: translate
  FTranslation[trNoConstants] := 'The units do not contain any constants.'; // TODO: translate
  FTranslation[trNoFunctions] := 'The units do not contain any functions or procedures.'; // TODO: translate
  FTranslation[trNoIdentifiers] := 'The units do not contain any identifiers.'; // TODO: translate
  FTranslation[trProgram] := 'Program'; // TODO: translate
end;

{ ---------------------------------------------------------------------------- }

{$I PasDoc_Languages_Chinese_gb2312.inc}

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageDanish;
begin
  FTranslation[trAuthor] := 'Forfatter';
  FTranslation[trAuthors] := 'Forfatre';
  FTranslation[trCio] := 'Klasser, interfaces og objekter';
  FTranslation[trClass] := 'Klasse';
  FTranslation[trClasses] := 'Klasser';
  FTranslation[trConstants] := 'Konstanter';
  FTranslation[trCreated] := 'Udført';
  FTranslation[trDeclaration] := 'Declaration';
  FTranslation[trDescription] := 'Beskrivelse';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions'; // TODO: translate
  FTranslation[trExceptionsRaised] := 'Exceptions raised'; // TODO: translate
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Felter';
  FTranslation[trFunctionsAndProcedures] := 'Funktioner og prosedurer';
  FTranslation[trHelp] := 'Hjælp';
  FTranslation[trHierarchy] := 'Herarki';
  FTranslation[trIdentifiers] := 'Identifiers';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLegend] := 'Legende';
  FTranslation[trMarker] := 'Marker'; // TODO: translate
  FTranslation[trVisibility] := 'Visibility'; // TODO: translate
  FTranslation[trLastModified] := 'Sidst Modificieret';
  FTranslation[trMethods] := 'Metoder';
  FTranslation[trName] := 'Navn';
  FTranslation[trNone] := 'Ingen';
  FTranslation[trObject] := 'Objekt';
  FTranslation[trObjects] := 'Objekter';
  FTranslation[trOverview] := 'Sammendrag';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Egenskaber';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Type';
  FTranslation[trTypes] := 'Typer';
  FTranslation[trUnit] := 'Unit';
  FTranslation[trUnits] := 'Units';
  FTranslation[trVariables] := 'Variable';
  FTranslation[trGvUses] := 'Unit dependency graph'; // TODO: translate
  FTranslation[trGvClasses] := 'Classes hierarchy graph'; // TODO: translate
  FTranslation[trWarningOverwrite] :=
    'Advarsel: Editer ikke denne fil, den er autogeneret og vil sansylgvis blive overskret';
  FTranslation[trWarning] := 'Warning';  // TODO: translate
  FTranslation[trHeadlineCio] := 'Alle Klasesr, Interfaces og Objekter';
  FTranslation[trHeadlineConstants] := 'Alle Konstanter';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Alle Functioner and Procedurer';
  FTranslation[trHeadlineIdentifiers] := 'Alle Identifiers';
  FTranslation[trHeadlineTypes] := 'Alle Typer';
  FTranslation[trHeadlineUnits] := 'Alle Units';
  FTranslation[trHeadlineVariables] := 'Alle Variable';
  FTranslation[trSummaryCio] :=
    'Oversigt over klasser, interfaces & objekter';
  FTranslation[trGeneratedBy] := 'Generated by'; // TODO: translate
  FTranslation[trOnDateTime] := 'on'; // TODO: translate
  FTranslation[trDeprecated] := 'this symbol is deprecated'; // TODO: translate
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform'; // TODO: translate
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library'; // TODO: translate
  FTranslation[trIntroduction] := 'Introduction'; // TODO: translate
  FTranslation[trConclusion] := 'Conclusion'; // TODO: translate
  FTranslation[trSearch] := 'Search'; // TODO: translate
  FTranslation[trSeeAlso] := 'See also'; // TODO: translate
  FTranslation[trValues] := 'Values'; // TODO: translate
  FTranslation[trNoCIOs] := 'The units do not contain any classes, interfaces, objects or records.'; // TODO: translate
  FTranslation[trNoCIOsForHierarchy] := 'The units do not contain any classes, interfaces or objects.'; // TODO: translate
  FTranslation[trNoTypes] := 'The units do not contain any types.'; // TODO: translate
  FTranslation[trNoVariables] := 'The units do not contain any variables.'; // TODO: translate
  FTranslation[trNoConstants] := 'The units do not contain any constants.'; // TODO: translate
  FTranslation[trNoFunctions] := 'The units do not contain any functions or procedures.'; // TODO: translate
  FTranslation[trNoIdentifiers] := 'The units do not contain any identifiers.'; // TODO: translate
  FTranslation[trProgram] := 'Program'; // TODO: translate
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageDutch;
begin
  FTranslation[trAuthor] := 'Auteur';
  FTranslation[trAuthors] := 'Auteurs';
  FTranslation[trCio] := 'Classes, interfaces and objecten';
  FTranslation[trClass] := 'Class';
  FTranslation[trClasses] := 'Classes';
  FTranslation[trConstants] := 'Constanten';
  FTranslation[trCreated] := 'Gemaakt';
  FTranslation[trDeclaration] := 'Declaratie';
  FTranslation[trDescription] := 'Omschrijving';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions'; // TODO: translate
  FTranslation[trExceptionsRaised] := 'Exceptions raised'; // TODO: translate
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Velden';
  FTranslation[trFunctionsAndProcedures] := 'Functies en procedures';
  FTranslation[trHelp] := 'Help';
  FTranslation[trHierarchy] := 'Hierarchie';
  FTranslation[trIdentifiers] := 'Identifiers';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLastModified] := 'Laatste wijziging';
  FTranslation[trLegend] := 'Legend';
  FTranslation[trMarker] := 'Marker'; // TODO: translate
  FTranslation[trVisibility] := 'Visibility'; // TODO: translate
  FTranslation[trMethods] := 'Methods';
  FTranslation[trName] := 'Naam';
  FTranslation[trNone] := 'Geen';
  FTranslation[trObject] := 'Object';
  FTranslation[trObjects] := 'Objecten';
  FTranslation[trOverview] := 'Overzicht';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Eigenschappen';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Type';
  FTranslation[trTypes] := 'Typen';
  FTranslation[trUnit] := 'Unit';
  FTranslation[trUnits] := 'Units';
  FTranslation[trVariables] := 'Variabelen';
  FTranslation[trGvUses] := 'Unit dependency graph'; // TODO: translate
  FTranslation[trGvClasses] := 'Classes hierarchy graph'; // TODO: translate
  FTranslation[trWarningOverwrite] :=
    'Waarschuwing, wijzig niets - dit bestand is automatisch gegenereerd en zal worden overschreven';
  FTranslation[trWarning] := 'Warning';  // TODO: translate
  FTranslation[trHeadlineCio] := 'Alle classes, interfaces en objecten';
  FTranslation[trHeadlineConstants] := 'Alle constanten';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Alle functies en procedures';
  FTranslation[trHeadlineIdentifiers] := 'Alle identifiers';
  FTranslation[trHeadlineTypes] := 'Alle typen';
  FTranslation[trHeadlineUnits] := 'Alle units';
  FTranslation[trHeadlineVariables] := 'Alle variabelen';
  FTranslation[trSummaryCio] :=
    'Overzicht van classes, interfaces & objecten';
  FTranslation[trGeneratedBy] := 'Generated by'; // TODO: translate
  FTranslation[trOnDateTime] := 'on'; // TODO: translate
  FTranslation[trDeprecated] := 'this symbol is deprecated'; // TODO: translate
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform'; // TODO: translate
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library'; // TODO: translate
  FTranslation[trIntroduction] := 'Introduction'; // TODO: translate
  FTranslation[trConclusion] := 'Conclusion'; // TODO: translate
  FTranslation[trSearch] := 'Search'; // TODO: translate
  FTranslation[trSeeAlso] := 'See also'; // TODO: translate
  FTranslation[trValues] := 'Values'; // TODO: translate
  FTranslation[trNoCIOs] := 'The units do not contain any classes, interfaces, objects or records.'; // TODO: translate
  FTranslation[trNoCIOsForHierarchy] := 'The units do not contain any classes, interfaces or objects.'; // TODO: translate
  FTranslation[trNoTypes] := 'The units do not contain any types.'; // TODO: translate
  FTranslation[trNoVariables] := 'The units do not contain any variables.'; // TODO: translate
  FTranslation[trNoConstants] := 'The units do not contain any constants.'; // TODO: translate
  FTranslation[trNoFunctions] := 'The units do not contain any functions or procedures.'; // TODO: translate
  FTranslation[trNoIdentifiers] := 'The units do not contain any identifiers.'; // TODO: translate
  FTranslation[trProgram] := 'Programma';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageFrench;
begin
  FTranslation[trAuthor] := 'Auteur';
  FTranslation[trAuthors] := 'Auteurs';
  FTranslation[trAutomated] := 'Automatisé';
  FTranslation[trCio] := 'Classes, interfaces, structures et objets';
  FTranslation[trClass] := 'Classe';
  FTranslation[trClasses] := 'Classes';
  FTranslation[trClassHierarchy] := 'Hiérarchie des classes';
  FTranslation[trConstants] := 'Constantes';
  FTranslation[trCreated] := 'Crée';
  FTranslation[trDeclaration] := 'Déclaration';
  FTranslation[trDescription] := 'Description';
  FTranslation[trParameters] := 'Paramètres';
  FTranslation[trReturns] := 'Retourne';
  FTranslation[trExceptions] := 'Exceptions'; // TODO: translate
  FTranslation[trExceptionsRaised] := 'Exceptions raised'; // TODO: translate
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Champs';
  FTranslation[trFunctionsAndProcedures] := 'Fonctions et procédures';
  FTranslation[trHelp] := 'Aide';
  FTranslation[trHierarchy] := 'Hiérarchie';
  FTranslation[trIdentifiers] := 'Identificateurs';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLegend] := 'Légende';
  FTranslation[trMarker] := 'Marquage';
  FTranslation[trVisibility] := 'Visibilité';
  FTranslation[trMethods] := 'Méthodes';
  FTranslation[trLastModified] := 'Dernière modification';
  FTranslation[trName] := 'Nom';
  FTranslation[trNone] := 'Aucun(e)(s)';
  FTranslation[trObject] := 'Objet';
  FTranslation[trObjects] := 'Objets';
  FTranslation[trOverview] := 'Aperçu';
  FTranslation[trPrivate] := 'Privé';
  FTranslation[trProperties] := 'Propriétés';
  FTranslation[trProtected] := 'Protégé';
  FTranslation[trPublic] := 'Public';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Publiés';
  FTranslation[trType] := 'Type';
  FTranslation[trTypes] := 'Types';
  FTranslation[trUnit] := 'Unité';
  FTranslation[trUnits] := 'Unités';
  FTranslation[trVariables] := 'Variables';
  FTranslation[trGvUses] := 'Graphique de dépendance d''unités';
  FTranslation[trGvClasses] := 'Graphique de hiérarchie des classes';
  FTranslation[trHeadlineCio] := 'Toutes les classes, interfaces, objets et enregistrements';
  FTranslation[trHeadlineConstants] := 'Toutes les constants';
  FTranslation[trHeadlineFunctionsAndProcedures] := 'Toutes les fonctions et procédures';
  FTranslation[trHeadlineIdentifiers] := 'Tous les identificateurs';
  FTranslation[trHeadlineTypes] := 'Tous les types';
  FTranslation[trHeadlineUnits] := 'Toutes les unités';
  FTranslation[trHeadlineVariables] := 'Toutes les variables';
  FTranslation[trSummaryCio] := 'Classes, interfaces, objets et enregistrements';
  FTranslation[trWarningOverwrite] :=
    'Attention, ne pas édtier - ce fichier est créé automatiquement et va être écrasé';
  FTranslation[trWarning] := 'Warning';  // TODO: translate
  FTranslation[trGeneratedBy] := 'Produit par';
  FTranslation[trOnDateTime] := 'le';
  FTranslation[trDeprecated] := 'ce symbole est désapprouvé';
  FTranslation[trPlatformSpecific] := 'ce symbole est spécifique à une plateforme d''exécution';
  FTranslation[trLibrarySpecific] := 'ce symbole est spécifique à une certaine bibliothèque';
  FTranslation[trIntroduction] := 'Introduction'; // TODO: translate
  FTranslation[trConclusion] := 'Conclusion'; // TODO: translate
  FTranslation[trSearch] := 'Recherce';
  FTranslation[trSeeAlso] := 'See also'; // TODO: translate
  FTranslation[trValues] := 'Values'; // TODO: translate
  FTranslation[trNoCIOs] := 'The units do not contain any classes, interfaces, objects or records.'; // TODO: translate
  FTranslation[trNoCIOsForHierarchy] := 'The units do not contain any classes, interfaces or objects.'; // TODO: translate
  FTranslation[trNoTypes] := 'The units do not contain any types.'; // TODO: translate
  FTranslation[trNoVariables] := 'The units do not contain any variables.'; // TODO: translate
  FTranslation[trNoConstants] := 'The units do not contain any constants.'; // TODO: translate
  FTranslation[trNoFunctions] := 'The units do not contain any functions or procedures.'; // TODO: translate
  FTranslation[trNoIdentifiers] := 'The units do not contain any identifiers.'; // TODO: translate
  FTranslation[trProgram] := 'Program'; // TODO: translate
end;

{ ---------------------------------------------------------------------------- }

{$IFDEF old}
procedure TPasDocLanguages.SetLanguageGerman;
begin
  FTranslation[trAuthor] := 'Autor';
  FTranslation[trAuthors] := 'Autoren';
  FTranslation[trAutomated] := 'Automated';
  FTranslation[trCio] := 'Klassen, Schnittstellen und Objekte';
  FTranslation[trClass] := 'Klasse';
  FTranslation[trClasses] := 'Klassen';
  FTranslation[trClassHierarchy] := 'Klassenhierarchie';
  FTranslation[trConstants] := 'Konstanten';
  FTranslation[trCreated] := 'Erstellt';
  FTranslation[trDeclaration] := 'Deklaration';
  FTranslation[trDescription] := 'Beschreibung';
  FTranslation[trParameters] := 'Parameter';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions'; // TODO: translate
  FTranslation[trExceptionsRaised] := 'Exceptions raised'; // TODO: translate
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Felder';
  FTranslation[trFunctionsAndProcedures] := 'Funktionen und Prozeduren';
  FTranslation[trHelp] := 'Hilfe';
  FTranslation[trHierarchy] := 'Hierarchie';
  FTranslation[trIdentifiers] := 'Bezeichner';
  FTranslation[trInterface] := 'Schnittstelle';
  FTranslation[trLegend] := 'Legende';
  FTranslation[trMarker] := 'Markierung';
  FTranslation[trVisibility] := 'Sichtbarkeit';
  FTranslation[trMethods] := 'Methoden';
  FTranslation[trLastModified] := 'Letzte Änderung';
  FTranslation[trName] := 'Name';
  FTranslation[trNone] := 'Keine';
  FTranslation[trObject] := 'Objekt';
  FTranslation[trObjects] := 'Objekte';
  FTranslation[trOverview] := 'Übersicht';
  FTranslation[trPrivate] := 'Privat';
  FTranslation[trProperties] := 'Eigenschaften';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Type';
  FTranslation[trTypes] := 'Typen';
  FTranslation[trUnit] := 'Unit';
  FTranslation[trUnits] := 'Units';
  FTranslation[trVariables] := 'Variablen';
  FTranslation[trGvUses] := 'Unit Abhängigkeitsgraph';
  FTranslation[trGvClasses] := 'Klassenhierarchie Graph';
  FTranslation[trHeadlineCio] := 'Alle Klassen, Schnittstellen, Objekte und Records';
  FTranslation[trHeadlineConstants] := 'Alle Konstanten';
  FTranslation[trHeadlineFunctionsAndProcedures] :=  'Alle Funktionen und Prozeduren';
  FTranslation[trHeadlineIdentifiers] := 'Alle Bezeichner';
  FTranslation[trHeadlineTypes] := 'Alle Typen';
  FTranslation[trHeadlineUnits] := 'Alle Units';
  FTranslation[trHeadlineVariables] := 'Alle Variablen';
  FTranslation[trSummaryCio] := 'Zusammenfassung aller Klassen, Schnittstellen, Objekte und Records';
  FTranslation[trWarningOverwrite] :=
    'Achtung: Nicht ändern - diese Datei wurde automatisch erstellt und wird möglicherweise überschrieben';
  FTranslation[trWarning] := 'Warning';  // TODO: translate
  FTranslation[trGeneratedBy] := 'Erstellt mit';
  FTranslation[trOnDateTime] := 'am';
  FTranslation[trDeprecated] := 'Dieses Symbol sollte nicht (mehr) verwendet werden.';
  FTranslation[trPlatformSpecific] := 'Dieses Symbol ist plattformspezifisch.';
  FTranslation[trLibrarySpecific] := 'Dieses Symbol ist spezifisch für eine bestimmte Bibliothek.';
  FTranslation[trIntroduction] := 'Einführung';
  FTranslation[trConclusion] := 'Fazit';
  FTranslation[trSearch] := 'Suchen';
  FTranslation[trSeeAlso] := 'See also'; // TODO: translate
  FTranslation[trValues] := 'Values'; // TODO: translate
  FTranslation[trNoCIOs] := 'The units do not contain any classes, interfaces, objects or records.'; // TODO: translate
  FTranslation[trNoCIOsForHierarchy] := 'The units do not contain any classes, interfaces or objects.'; // TODO: translate
  FTranslation[trNoTypes] := 'The units do not contain any types.'; // TODO: translate
  FTranslation[trNoVariables] := 'The units do not contain any variables.'; // TODO: translate
  FTranslation[trNoConstants] := 'The units do not contain any constants.'; // TODO: translate
  FTranslation[trNoFunctions] := 'The units do not contain any functions or procedures.'; // TODO: translate
  FTranslation[trNoIdentifiers] := 'The units do not contain any identifiers.'; // TODO: translate
  FTranslation[trProgram] := 'Programm';
end;
{$ELSE}
const
  aGerman: RTransTable = (
    {trNoTrans} '<häh?>', //not ID assigned, so far
  //map
    {trUnits} strKeep, //'Units',
    {trClassHierarchy} 'Klassenhierarchie',
    {trCio} 'Klassen, Interfaces und Objects',
    {trIdentifiers} 'Bezeichner',
    {trGvUses} 'Graph der Unit-Abhängigkeiten',
    {trGvClasses} 'Graph der Klassenhierarchie',
  //tables and members
    {trClasses} 'Klassen',
      {trClass} 'Klasse',
      {trDispInterface} strKeep, //'DispInterface',
      {trInterface} strKeep, //'Interface', 'Schnittstelle'?
    {trObjects} strKeep, //'Objects',
      {trObject} strKeep, //'Object',
      {trRecord} strKeep, //'Record',
        {trHierarchy} 'Hierarchie',
        {trFields} 'Felder',
        {trMethods} 'Methoden',
        {trProperties} 'Eigenschaften',
    {trLibrary} 'Bibliothek',
    {trPackage} strKeep, //'Package',
    {trProgram} 'Programm',
    {trUnit} strKeep, //'Unit',
      {trUses} strKeep, //'Uses',
      {trConstants} 'Konstanten',
      {trFunctionsAndProcedures} 'Funktionen und Prozeduren',
      {trTypes} 'Datentypen',
        {trType} strKeep, //'Type', 'Typ'?
      {trVariables} 'Variablen',
      {trAuthors} 'Autoren',
        {trAuthor} 'Autor',
      {trCreated} 'Erstellt',
      {trLastModified} 'Letzte Änderung',
    {trSubroutine} 'Unterprogramm',
      {trParameters} 'Parameter',
      {trReturns} 'Result',
      {trExceptionsRaised} 'Wirft Ausnahmen', //'Exceptions raised',
    {trExceptions} 'Ausnahmen',
      {trException} strKeep, //'Exception',
    {trEnum} strKeep, //'Enumeration',
  //visibilities
    {trVisibility} 'Sichtbarkeit',
      {trPrivate} strKeep, //'Private',
      {trStrictPrivate} strKeep, //'Strict Private',
      {trProtected} strKeep, //'Protected',
      {trStrictProtected} strKeep, //'Strict Protected',
      {trPublic} strKeep, //'Public',
      {trPublished} strKeep, //'Published',
      {trAutomated} strKeep, //'Automated',
      {trImplicit} strKeep, //'Implicit',
  //hints
    {trDeprecated} 'Dieses Symbol sollte nicht (mehr) verwendet werden.',
    {trPlatformSpecific} 'Dieses Symbol ist plattformspezifisch.',
    {trLibrarySpecific} 'Dieses Symbol ist spezifisch für eine bestimmte Bibliothek.',
  //headings
    {trOverview} 'Übersicht',
    {trIntroduction} 'Einführung',
    {trConclusion} 'Fazit',
    {trHeadlineCio} 'Alle Klassen, Schnittstellen, Objekte und Records',
    {trHeadlineConstants} 'Alle Konstanten',
    {trHeadlineFunctionsAndProcedures} 'Alle Funktionen und Prozeduren',
    {trHeadlineIdentifiers} 'Alle Bezeichner',
    {trHeadlineTypes} 'Alle Typen',
    {trHeadlineUnits} 'Alle Units',
    {trHeadlineVariables} 'Alle Variablen',
    {trSummaryCio} 'Zusammenfassung aller Klassen, Schnittstellen, Objekte und Records',
  //column headings
    {trDeclaration} 'Deklaration',
    {trDescription} 'Beschreibung',
    {trName} strKeep, //'Name',
    {trValues} 'Werte',
  //empty
    {trNone} 'Keine',
    {trNoCIOs} 'Die Units enthalten keine Klassen, Interfaces, Objects oder Records.',
    {trNoCIOsForHierarchy} 'Die Units enthalten keine Klassen, Interfaces oder Objects.',
    {trNoTypes} 'Die Units enthalten keine Typen.',
    {trNoVariables} 'Die Units enthalten keine Variablen.',
    {trNoConstants} 'Die Units enthalten keine Konstanten.',
    {trNoFunctions} 'Die Units enthalten keine Funktionen oder Prozeduren.',
    {trNoIdentifiers} 'Die Units enthalten keine Bezeichner.',
  //misc
    {trHelp} 'Hilfe',
    {trLegend} 'Legende',
    {trMarker} 'Markierung',
    {trWarningOverwrite} 'Achtung: Nicht ändern - diese Datei wurde automatisch erstellt und wird möglicherweise überschrieben',
    {trWarning} 'Warnung',
    {trGeneratedBy} 'Erstellt mit',
    {trOnDateTime} 'am',
    {trSearch} 'Suchen',
    {trSeeAlso} 'Siehe auch',
    ''  //dummy
  );
{$ENDIF}
{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageIndonesian;
begin
  FTranslation[trAuthor] := 'Pembuat';
  FTranslation[trAuthors] := 'Pembuat';
  FTranslation[trCio] := 'Kelas, Interface, dan Objek';
  FTranslation[trClass] := 'Kelas';
  FTranslation[trClasses] := 'Kelas';
  FTranslation[trConstants] := 'Konstanta';
  FTranslation[trCreated] := 'Dibuat';
  FTranslation[trDeclaration] := 'Deklarasi';
  FTranslation[trDescription] := 'Definisi';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions'; // TODO: translate
  FTranslation[trExceptionsRaised] := 'Exceptions raised'; // TODO: translate
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Field';
  FTranslation[trFunctionsAndProcedures] := 'Fungsi dan Prosedur';
  FTranslation[trHelp] := 'Bantuan';
  FTranslation[trHierarchy] := 'Hirarki';
  FTranslation[trIdentifiers] := 'Identifier';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLegend] := 'Legenda';
  FTranslation[trMarker] := 'Marker'; // TODO: translate
  FTranslation[trVisibility] := 'Visibility'; // TODO: translate
  FTranslation[trMethods] := 'Method';
  FTranslation[trLastModified] := 'Terakhir Dimodifikasi';
  FTranslation[trName] := 'Nama';
  FTranslation[trNone] := 'Tidak Ada';
  FTranslation[trObject] := 'Objek';
  FTranslation[trObjects] := 'Objek';
  FTranslation[trOverview] := 'Sekilas';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Property';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Tipe Bentukan';
  FTranslation[trTypes] := 'Tipe Bentukan';
  FTranslation[trUnit] := 'Unit';
  FTranslation[trUnits] := 'Unit';
  FTranslation[trVariables] := 'Variabel';
  FTranslation[trGvUses] := 'Unit dependency graph'; // TODO: translate
  FTranslation[trGvClasses] := 'Classes hierarchy graph'; // TODO: translate
  FTranslation[trHeadlineCio] := 'Semua Kelas, Interface, dan Objek';
  FTranslation[trHeadlineConstants] := 'Semua Konstanta';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Semua Fungsi dan Prosedur';
  FTranslation[trHeadlineIdentifiers] := 'Semua Identifier';
  FTranslation[trHeadlineTypes] := 'Semua Tipe Bentukan';
  FTranslation[trHeadlineUnits] := 'Semua Unit';
  FTranslation[trHeadlineVariables] := 'Semua Variabel';
  FTranslation[trSummaryCio] := 'Ringkasan Kelas, Interface, dan Objek';
  FTranslation[trWarningOverwrite] := 'Perhatian: Jangan dimodifikasi - '
    + 'file ini dihasilkan secara otomatis dan mungkin saja ditimpa ulang';
  FTranslation[trWarning] := 'Warning';  // TODO: translate
  FTranslation[trGeneratedBy] := 'Dihasilkan oleh';
  FTranslation[trOnDateTime] := 'pada';
  FTranslation[trDeprecated] := 'this symbol is deprecated'; // TODO: translate
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform'; // TODO: translate
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library';  // TODO: translate
  FTranslation[trIntroduction] := 'Introduction'; // TODO: translate
  FTranslation[trConclusion] := 'Conclusion'; // TODO: translate
  FTranslation[trSearch] := 'Search'; // TODO: translate
  FTranslation[trSeeAlso] := 'See also'; // TODO: translate
  FTranslation[trValues] := 'Values'; // TODO: translate
  FTranslation[trNoCIOs] := 'The units do not contain any classes, interfaces, objects or records.'; // TODO: translate
  FTranslation[trNoCIOsForHierarchy] := 'The units do not contain any classes, interfaces or objects.'; // TODO: translate
  FTranslation[trNoTypes] := 'The units do not contain any types.'; // TODO: translate
  FTranslation[trNoVariables] := 'The units do not contain any variables.'; // TODO: translate
  FTranslation[trNoConstants] := 'The units do not contain any constants.'; // TODO: translate
  FTranslation[trNoFunctions] := 'The units do not contain any functions or procedures.'; // TODO: translate
  FTranslation[trNoIdentifiers] := 'The units do not contain any identifiers.'; // TODO: translate
  FTranslation[trProgram] := 'Program'; // TODO: translate
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageItalian;
begin
  FTranslation[trAuthor] := 'Autore';
  FTranslation[trAuthors] := 'Autori';
  FTranslation[trCio] := 'Classi, Interfacce ed Oggetti';
  FTranslation[trClass] := 'Classe';
  FTranslation[trClasses] := 'Classi';
  FTranslation[trConstants] := 'Costanti';
  FTranslation[trCreated] := 'Creato';
  FTranslation[trDeclaration] := 'Dichiarazione';
  FTranslation[trDescription] := 'Descrizione';
  FTranslation[trParameters] := 'Parametri';
  FTranslation[trReturns] := 'Ritorni';
  FTranslation[trExceptions] := 'Eccezione';
  FTranslation[trExceptionsRaised] := 'Eccezioni sollevate';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Campi';
  FTranslation[trFunctionsAndProcedures] := 'Funzioni e Procedure';
  FTranslation[trHelp] := 'Help';
  FTranslation[trHierarchy] := 'Gerarchia';
  FTranslation[trIdentifiers] := 'Identificatori';
  FTranslation[trInterface] := 'Interfacce';
  FTranslation[trLegend] := 'Legenda';
  FTranslation[trMarker] := 'Marker'; // TODO: translate
  FTranslation[trVisibility] := 'Visibility'; // TODO: translate
  FTranslation[trMethods] := 'Metodi';
  FTranslation[trLastModified] := 'Ultima Variazione';
  FTranslation[trName] := 'Nome';
  FTranslation[trNone] := 'Nessuno';
  FTranslation[trObject] := 'Oggetto';
  FTranslation[trObjects] := 'Oggetti';
  FTranslation[trOverview] := 'Sommario';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Proprietà';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Tipo';
  FTranslation[trTypes] := 'Tipi';
  FTranslation[trUnit] := 'Unit';
  FTranslation[trUnits] := 'Units';
  FTranslation[trVariables] := 'Variabili';
  FTranslation[trGvUses] := 'Grafico dipendenze Unit';
  FTranslation[trGvClasses] := 'Grafico gerarchia Classi';
  FTranslation[trHeadlineCio] := 'Tutte le Classi, Interfacce ed Oggetti';
  FTranslation[trHeadlineConstants] := 'Tutte le Costanti';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Tutte le Funzioni e Procedure';
  FTranslation[trHeadlineIdentifiers] := 'Tutti gli Identificatori';
  FTranslation[trHeadlineTypes] := 'Tutti i Tipi';
  FTranslation[trHeadlineUnits] := 'Tutte le Units';
  FTranslation[trHeadlineVariables] := 'Tutte le Variabili';
  FTranslation[trSummaryCio] := 'Sommario di Classi, Interfacce ed Oggetti';
  FTranslation[trWarningOverwrite] :=
    'Attenzione: Non modificare - questo file è stato generato automaticamente e verrà probabilmente sovrascritto';
  FTranslation[trWarning] := 'Warning';  // TODO: translate
  FTranslation[trGeneratedBy] := 'Generated by'; // TODO: translate
  FTranslation[trOnDateTime] := 'on'; // TODO: translate
  FTranslation[trDeprecated] := 'this symbol is deprecated'; // TODO: translate
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform'; // TODO: translate
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library'; // TODO: translate
  FTranslation[trIntroduction] := 'Introduczione';
  FTranslation[trConclusion] := 'Conclusion'; // TODO: translate
  FTranslation[trSearch] := 'Cerca';
  FTranslation[trSeeAlso] := 'Vedere Anche';
  FTranslation[trValues] := 'Valori';
  FTranslation[trNoCIOs] := 'The units do not contain any classes, interfaces, objects or records.'; // TODO: translate
  FTranslation[trNoCIOsForHierarchy] := 'The units do not contain any classes, interfaces or objects.'; // TODO: translate
  FTranslation[trNoTypes] := 'The units do not contain any types.'; // TODO: translate
  FTranslation[trNoVariables] := 'The units do not contain any variables.'; // TODO: translate
  FTranslation[trNoConstants] := 'The units do not contain any constants.'; // TODO: translate
  FTranslation[trNoFunctions] := 'The units do not contain any functions or procedures.'; // TODO: translate
  FTranslation[trNoIdentifiers] := 'The units do not contain any identifiers.'; // TODO: translate
  FTranslation[trProgram] := 'Program'; // TODO: translate
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageJavanese;
begin
  FTranslation[trAuthor] := 'Sing Nggawe';
  FTranslation[trAuthors] := 'Sing Nggawe';
  FTranslation[trCio] := 'Kelas, Interface, lan Objek';
  FTranslation[trClass] := 'Kelas';
  FTranslation[trClasses] := 'Kelas';
  FTranslation[trConstants] := 'Konstanta';
  FTranslation[trCreated] := 'Digawe';
  FTranslation[trDeclaration] := 'Deklarasi';
  FTranslation[trDescription] := 'Katrangan';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions'; // TODO: translate
  FTranslation[trExceptionsRaised] := 'Exceptions raised'; // TODO: translate
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Field';
  FTranslation[trFunctionsAndProcedures] := 'Fungsi lan Prosedur';
  FTranslation[trHelp] := 'Tulung';
  FTranslation[trHierarchy] := 'Hirarki';
  FTranslation[trIdentifiers] := 'Identifier';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLegend] := 'Katrangan';
  FTranslation[trMarker] := 'Marker'; // TODO: translate
  FTranslation[trVisibility] := 'Visibility'; // TODO: translate
  FTranslation[trMethods] := 'Method';
  FTranslation[trLastModified] := 'Terakhir Diowahi';
  FTranslation[trName] := 'Jeneng';
  FTranslation[trNone] := 'Mboten Wonten';
  FTranslation[trObject] := 'Objek';
  FTranslation[trObjects] := 'Objek';
  FTranslation[trOverview] := 'Pambuka';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Property';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Macem Gawean';
  FTranslation[trTypes] := 'Macem Gawean';
  FTranslation[trUnit] := 'Unit';
  FTranslation[trUnits] := 'Unit';
  FTranslation[trVariables] := 'Variabel';
  FTranslation[trGvUses] := 'Unit dependency graph'; // TODO: translate
  FTranslation[trGvClasses] := 'Classes hierarchy graph'; // TODO: translate
  FTranslation[trHeadlineCio] := 'Kabeh Kelas, Interface, lan Objek';
  FTranslation[trHeadlineConstants] := 'Kabeh Konstanta';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Kabeh Fungsi lan Prosedur';
  FTranslation[trHeadlineIdentifiers] := 'Kabeh Identifier';
  FTranslation[trHeadlineTypes] := 'Kabeh Macem Gawean';
  FTranslation[trHeadlineUnits] := 'Kabeh Unit';
  FTranslation[trHeadlineVariables] := 'Kabeh Variabel';
  FTranslation[trSummaryCio] := 'Ringkesan Kelas, Interface, lan Objek';
  FTranslation[trWarningOverwrite] := 'Ati-ati: Ojo diowahi - '
    + 'file iki digawe otomatis dadi iso ilang owahanmu';
  FTranslation[trWarning] := 'Warning';  // TODO: translate
  FTranslation[trGeneratedBy] := 'Dihasilne karo';
  FTranslation[trOnDateTime] := 'ing';
  FTranslation[trDeprecated] := 'this symbol is deprecated'; // TODO: translate
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform'; // TODO: translate
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library'; // TODO: translate
  FTranslation[trIntroduction] := 'Introduction'; // TODO: translate
  FTranslation[trConclusion] := 'Conclusion'; // TODO: translate
  FTranslation[trSearch] := 'Search'; // TODO: translate
  FTranslation[trSeeAlso] := 'See also'; // TODO: translate
  FTranslation[trValues] := 'Values'; // TODO: translate
  FTranslation[trNoCIOs] := 'The units do not contain any classes, interfaces, objects or records.'; // TODO: translate
  FTranslation[trNoCIOsForHierarchy] := 'The units do not contain any classes, interfaces or objects.'; // TODO: translate
  FTranslation[trNoTypes] := 'The units do not contain any types.'; // TODO: translate
  FTranslation[trNoVariables] := 'The units do not contain any variables.'; // TODO: translate
  FTranslation[trNoConstants] := 'The units do not contain any constants.'; // TODO: translate
  FTranslation[trNoFunctions] := 'The units do not contain any functions or procedures.'; // TODO: translate
  FTranslation[trNoIdentifiers] := 'The units do not contain any identifiers.'; // TODO: translate
  FTranslation[trProgram] := 'Program'; // TODO: translate
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguagePolish_CP1250;
begin
  FTranslation[trAuthor] := 'Autor';
  FTranslation[trAuthors] := 'Autorzy';
  FTranslation[trAutomated] := 'Automated';
  FTranslation[trCio] := 'Klasy, interfejsy, obiekty i rekordy';
  FTranslation[trClass] := 'Klasa';
  FTranslation[trClasses] := 'Klasy';
  FTranslation[trClassHierarchy] := 'Hierarchia klas';
  FTranslation[trConstants] := 'Sta³e';
  FTranslation[trCreated] := 'Utworzony';
  FTranslation[trDeclaration] := 'Deklaracja';
  FTranslation[trDescription] := 'Opis';
  FTranslation[trParameters] := 'Parametry';
  FTranslation[trReturns] := 'Wynik';
  FTranslation[trExceptions] := 'Wyj¹tki';
  FTranslation[trExceptionsRaised] := 'Generowane wyj¹tki';
  FTranslation[trEnum] := 'Wyliczenie';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Pola';
  FTranslation[trFunctionsAndProcedures] := 'Podprogramy';
  FTranslation[trHelp] := 'Pomoc';
  FTranslation[trHierarchy] := 'Hierarchia';
  FTranslation[trIdentifiers] := 'Identyfikatory';
  FTranslation[trInterface] := 'Interfejs';
  FTranslation[trLegend] := 'Legenda';
  FTranslation[trMarker] := 'Kolor';
  FTranslation[trVisibility] := 'Widocznoœæ';
  FTranslation[trMethods] := 'Metody';
  FTranslation[trLastModified] := 'Ostatnia modyfikacja';
  FTranslation[trName] := 'Nazwa';
  FTranslation[trNone] := 'Brak';
  FTranslation[trObject] := 'Obiekt';
  FTranslation[trObjects] := 'Obiekty';
  FTranslation[trOverview] := 'Przegl¹d';
  FTranslation[trPrivate] := 'Prywatne';
  FTranslation[trProperties] := 'W³aœciwoœci';
  FTranslation[trProtected] := 'Chronione';
  FTranslation[trPublic] := 'Publiczne';
  FTranslation[trImplicit] := 'Domyœlne';
  FTranslation[trPublished] := 'Publikowane';
  FTranslation[trType] := 'Typ';
  FTranslation[trTypes] := 'Typy';
  FTranslation[trUnit] := 'Modu³';
  FTranslation[trUnits] := 'Modu³y';
  FTranslation[trVariables] := 'Zmienne';
  FTranslation[trGvUses] := 'Graf zale¿noœci modu³ów';
  FTranslation[trGvClasses] := 'Graf dziedziczenia klas';
  FTranslation[trHeadlineCio] := 'Wszystkie klasy, interfejsy, obiekty i rekordy';
  FTranslation[trHeadlineConstants] := 'Wszystkie sta³e';
  FTranslation[trHeadlineFunctionsAndProcedures] := 'Wszystkie podprogramy';
  FTranslation[trHeadlineIdentifiers] := 'Wszystkie identyfikatory';
  FTranslation[trHeadlineTypes] := 'Wszystkie typy';
  FTranslation[trHeadlineUnits] := 'Wszystkie modu³y';
  FTranslation[trHeadlineVariables] := 'Wszystkie zmienne';
  FTranslation[trSummaryCio] := 
    'Podsumowanie klas, interfejsów, obiektów i rekordów';
  FTranslation[trWarningOverwrite] :=
    'Uwaga, nie modyfikuj - ten plik zosta³ wygenerowany automatycznie i mo¿e zostaæ nadpisany';
  FTranslation[trWarning] := 'Uwaga';
  FTranslation[trGeneratedBy] := 'Wygenerowane przez';
  FTranslation[trOnDateTime] := ' - ';
  FTranslation[trDeprecated] := 'odradza siê u¿ywania tego identyfikatora';
  FTranslation[trPlatformSpecific] := 'ten identyfikator jest zale¿ny od platformy';
  FTranslation[trLibrarySpecific] := 'ten identyfikator jest zale¿ny od biblioteki';
  FTranslation[trIntroduction] := 'Wstêp';
  FTranslation[trConclusion] := 'Podsumowanie';
  FTranslation[trSearch] := 'Szukaj';
  FTranslation[trSeeAlso] := 'Zobacz tak¿e';
  FTranslation[trValues] := 'Wartoœci';
  FTranslation[trNoCIOs] := 'Modu³ nie zawiera ¿adnych klas, interfejsów, obiektów ani rekordów.';
  FTranslation[trNoCIOsForHierarchy] := 'Modu³ nie zawiera ¿adnych klas, interfejsów ani obiektów.';
  FTranslation[trNoTypes] := 'Modu³ nie zawiera ¿adnych typów.';
  FTranslation[trNoVariables] := 'Modu³ nie zawiera ¿adnych zmiennych.';
  FTranslation[trNoConstants] := 'Modu³ nie zawiera ¿adnych sta³ych.';
  FTranslation[trNoFunctions] := 'Modu³ nie zawiera ¿adnych funkcji ani podprogramów.';
  FTranslation[trNoIdentifiers] := 'Modu³ nie zawiera ¿adnych identyfikatorów.';
  FTranslation[trProgram] := 'Program';
end;

procedure TPasDocLanguages.SetLanguagePolish_ISO_8859_2;
begin
  FTranslation[trAuthor] := 'Autor';
  FTranslation[trAuthors] := 'Autorzy';
  FTranslation[trAutomated] := 'Automated';
  FTranslation[trCio] := 'Klasy, interfejsy, obiekty i rekordy';
  FTranslation[trClass] := 'Klasa';
  FTranslation[trClasses] := 'Klasy';
  FTranslation[trClassHierarchy] := 'Hierarchia klas';
  FTranslation[trConstants] := 'Sta³e';
  FTranslation[trCreated] := 'Utworzony';
  FTranslation[trDeclaration] := 'Deklaracja';
  FTranslation[trDescription] := 'Opis';
  FTranslation[trParameters] := 'Parametry';
  FTranslation[trReturns] := 'Wynik';
  FTranslation[trExceptions] := 'Wyj±tki';
  FTranslation[trExceptionsRaised] := 'Generowane wyj±tki';
  FTranslation[trEnum] := 'Wyliczenie';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Pola';
  FTranslation[trFunctionsAndProcedures] := 'Podprogramy';
  FTranslation[trHelp] := 'Pomoc';
  FTranslation[trHierarchy] := 'Hierarchia';
  FTranslation[trIdentifiers] := 'Identyfikatory';
  FTranslation[trInterface] := 'Interfejs';
  FTranslation[trLegend] := 'Legenda';
  FTranslation[trMarker] := 'Kolor';
  FTranslation[trVisibility] := 'Widoczno¶æ';
  FTranslation[trMethods] := 'Metody';
  FTranslation[trLastModified] := 'Ostatnia modyfikacja';
  FTranslation[trName] := 'Nazwa';
  FTranslation[trNone] := 'Brak';
  FTranslation[trObject] := 'Obiekt';
  FTranslation[trObjects] := 'Obiekty';
  FTranslation[trOverview] := 'Przegl±d';
  FTranslation[trPrivate] := 'Prywatne';
  FTranslation[trProperties] := 'W³a¶ciwo¶ci';
  FTranslation[trProtected] := 'Chronione';
  FTranslation[trPublic] := 'Publiczne';
  FTranslation[trImplicit] := 'Domy¶lne';
  FTranslation[trPublished] := 'Publikowane';
  FTranslation[trType] := 'Typ';
  FTranslation[trTypes] := 'Typy';
  FTranslation[trUnit] := 'Modu³';
  FTranslation[trUnits] := 'Modu³y';
  FTranslation[trVariables] := 'Zmienne';
  FTranslation[trGvUses] := 'Graf zale¿no¶ci modu³ów';
  FTranslation[trGvClasses] := 'Graf dziedziczenia klas';
  FTranslation[trHeadlineCio] := 'Wszystkie klasy, interfejsy, obiekty i rekordy';
  FTranslation[trHeadlineConstants] := 'Wszystkie sta³e';
  FTranslation[trHeadlineFunctionsAndProcedures] := 'Wszystkie podprogramy';
  FTranslation[trHeadlineIdentifiers] := 'Wszystkie identyfikatory';
  FTranslation[trHeadlineTypes] := 'Wszystkie typy';
  FTranslation[trHeadlineUnits] := 'Wszystkie modu³y';
  FTranslation[trHeadlineVariables] := 'Wszystkie zmienne';
  FTranslation[trSummaryCio] :=
    'Podsumowanie klas, interfejsów, obiektów i rekordów';
  FTranslation[trWarningOverwrite] :=
    'Uwaga, nie modyfikuj - ten plik zosta³ wygenerowany automatycznie i mo¿e zostaæ nadpisany';
  FTranslation[trWarning] := 'Uwaga';
  FTranslation[trGeneratedBy] := 'Wygenerowane przez';
  FTranslation[trOnDateTime] := ' - ';
  FTranslation[trDeprecated] := 'odradza siê u¿ywania tego identyfikatora';
  FTranslation[trPlatformSpecific] := 'ten identyfikator jest zale¿ny od platformy';
  FTranslation[trLibrarySpecific] := 'ten identyfikator jest zale¿ny od biblioteki';
  FTranslation[trIntroduction] := 'Wstêp';
  FTranslation[trConclusion] := 'Podsumowanie';
  FTranslation[trSearch] := 'Szukaj';
  FTranslation[trSeeAlso] := 'Zobacz tak¿e';
  FTranslation[trValues] := 'Warto¶ci';
  FTranslation[trNoCIOs] := 'Modu³ nie zawiera ¿adnych klas, interfejsów, obiektów ani rekordów.';
  FTranslation[trNoCIOsForHierarchy] := 'Modu³ nie zawiera ¿adnych klas, interfejsów ani obiektów.';
  FTranslation[trNoTypes] := 'Modu³ nie zawiera ¿adnych typów.';
  FTranslation[trNoVariables] := 'Modu³ nie zawiera ¿adnych zmiennych.';
  FTranslation[trNoConstants] := 'Modu³ nie zawiera ¿adnych sta³ych.';
  FTranslation[trNoFunctions] := 'Modu³ nie zawiera ¿adnych funkcji ani podprogramów.';
  FTranslation[trNoIdentifiers] := 'Modu³ nie zawiera ¿adnych identyfikatorów.';
  FTranslation[trProgram] := 'Program';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageRussian_1251;
begin
  FTranslation[trAuthor] := 'Àâòîğ';
  FTranslation[trAuthors] := 'Àâòîğû';
  FTranslation[trCio] := 'Êëàññû, èíòåğôåéñû è îáúåêòû';
  FTranslation[trClass] := 'Êëàññ';
  FTranslation[trClasses] := 'Êëàññû';
  FTranslation[trConstants] := 'Êîíñòàíòû';
  FTranslation[trCreated] := 'Ñîçäàíî';
  FTranslation[trDeclaration] := 'Îáúÿâëåíèÿ';
  FTranslation[trParameters] := 'Ïàğàìåòğû'; // DONE: translate
  FTranslation[trReturns] := 'Âîçâğàùàåìûå çíà÷åíèÿ'; // DONE: translate
  FTranslation[trExceptions] := 'Èñêëş÷åíèÿ'; // DONE: translate
  FTranslation[trExceptionsRaised] := 'Âûçûâàåò èñêëş÷åíèÿ'; // DONE: translate
  FTranslation[trEnum] := 'Ïåğå÷èñëåíèå';
  FTranslation[trDescription] := 'Îïèñàíèå';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Ïîëÿ';
  FTranslation[trFunctionsAndProcedures] := 'Ïğîöåäóğû è ôóíêöèè';
  FTranslation[trHelp] := 'Help';
    // Untranslated to avoid Russian file name for css
  FTranslation[trHierarchy] := 'Èåğàğõèÿ';
  FTranslation[trIdentifiers] := 'Èäåíòèôèêàòîğû';
  FTranslation[trInterface] := 'Èíòåğôåéñ';
  FTranslation[trLegend] := 'Îáîçíà÷åíèÿ';
  FTranslation[trMarker] := 'Ìàğêåğ'; // DONE: translate
  FTranslation[trVisibility] := 'Çîíà âèäèìîñòè'; // DONE: translate
  FTranslation[trLastModified] := 'Ïîñëåäíåå èçìåíåíèå';
  FTranslation[trMethods] := 'Ìåòîäû';
  FTranslation[trName] := 'Èìÿ';
  FTranslation[trNone] := 'Íåò';
  FTranslation[trObject] := 'Îáúåêò';
  FTranslation[trObjects] := 'Îáúåêòû';
  FTranslation[trOverview] := 'Îáçîğ';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Ñâîéñòâà';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Òèï';
  FTranslation[trTypes] := 'Òèïû';
  FTranslation[trUnit] := 'Ìîäóëü';
  FTranslation[trUnits] := 'Ìîäóëè';
  FTranslation[trVariables] := 'Ïåğåìåííûå';
  FTranslation[trGvUses] := 'Ãğàôèê çàâèñèìîñòè ìîäóëåé'; // DONE: translate
  FTranslation[trGvClasses] := 'Ãğàôèê èåğàğõèè êëàññîâ'; // DONE: translate
  FTranslation[trWarningOverwrite] :=
    'Ïğåäóïğåæäåíèå: íå ğåäàêòèğîâàòü - ıòîò ôàéë ñîçäàí àâòîìàòè÷åñêè è ìîæåò áûòü èçìåí¸í áåç ïğåäóïğåæäåíèÿ';
  FTranslation[trWarning] := 'Ïğåäóïğåæäåíèå';  // DONE: translate
  FTranslation[trHeadlineCio] := 'Âñå êëàññû, èíòåğôåéñû è îáúåêòû';
  FTranslation[trHeadlineConstants] := 'Âñå êîíñòàíòû';
  FTranslation[trHeadlineFunctionsAndProcedures] := 'Âñå ïğîöåäóğû è ôóíêöèè';
  FTranslation[trHeadlineIdentifiers] := 'Âñå èäåíòèôèêàòîğû';
  FTranslation[trHeadlineTypes] := 'Âñå òèïû';
  FTranslation[trHeadlineUnits] := 'Âñå ìîäóëè';
  FTranslation[trHeadlineVariables] := 'Âñå ïåğåìåííûå';
  FTranslation[trSummaryCio] := 'Ñïèñîê êëàññîâ, èíòåğôåéñîâ è îáúåêòîâ';
  FTranslation[trGeneratedBy] := 'Ñãåíåğèğîâàë '; // DONE: translate
  FTranslation[trOnDateTime] := 'äàòà/âğåìÿ'; // DONE: translate
  FTranslation[trDeprecated] := 'ıòîò ñèìâîë áîëüøå íå èñïîëüçóåòñÿ'; // DONE: translate
  FTranslation[trPlatformSpecific] := 'ıòîò ñèìâîë çàâèñèò îò ïëàòôîğìû'; // DONE: translate
  FTranslation[trLibrarySpecific] := 'ıòîò ñèìâîë çàâèñèò îò áèáëèîòåêè'; // DONE: translate
  FTranslation[trIntroduction] := 'Ââåäåíèå'; // DONE: translate
  FTranslation[trConclusion] := 'Çàêëş÷åíèå'; // DONE: translate
  FTranslation[trSearch] := 'Íàéòè'; // DONE: translate
  FTranslation[trSeeAlso] := 'Ìàòåğèàëû ïî òåìå'; // DONE: translate
  FTranslation[trValues] := 'Çíà÷åíèå'; // DONE: translate
  FTranslation[trNoCIOs] := 'Ìîäóëè íå ñîäåğæàò êëàññîâ, èíòåğôåéñîâ, îáúåêòîâ è çàïèñåé.'; // DONE: translate
  FTranslation[trNoCIOsForHierarchy] := 'Ìîäóëè íå ñîäåğæàò êëàññîâ, èíòåğôåéñîâ è îáúåêòîâ.'; // DONE: translate
  FTranslation[trNoTypes] := 'Ìîäóëè íå ñîäåğæàò òèïîâ.'; // DONE: translate
  FTranslation[trNoVariables] := 'Ìîäóëè íå ñîäåğæàò ïåğåìåííûõ.'; // DONE: translate
  FTranslation[trNoConstants] := 'Ìîäóëè íå ñîäåğæàò êîíñòàíò.'; // DONE: translate
  FTranslation[trNoFunctions] := 'Ìîäóëè íå ñîäåğæàò ôóíêöèè è ïğîöåäóğû.'; // DONE: translate
  FTranslation[trNoIdentifiers] := 'Ìîäóëè íå ñîäåğæàò íè îäíîãî èäåíòèôèêàòîğà.'; // DONE: translate
  FTranslation[trProgram] := 'Ïğîãğàììà'; // DONE: translate
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageRussian_866;
begin
  FTranslation[trAuthor] := '€¢â®à';
  FTranslation[trAuthors] := '€¢â®àë';
  FTranslation[trCio] := 'Š« ááë, ¨­â¥àä¥©áë ¨ ®¡ê¥ªâë';
  FTranslation[trClass] := 'Š« áá';
  FTranslation[trClasses] := 'Š« ááë';
  FTranslation[trConstants] := 'Š®­áâ ­âë';
  FTranslation[trCreated] := '‘®§¤ ­®';
  FTranslation[trDeclaration] := '¡êï¢«¥­¨ï';
  FTranslation[trParameters] := ' à ¬¥âàë'; // DONE: translate
  FTranslation[trReturns] := '‚®§¢à é ¥¬ë¥ §­ ç¥­¨ï'; // DONE: translate
  FTranslation[trExceptions] := 'ˆáª«îç¥­¨ï'; // DONE: translate
  FTranslation[trExceptionsRaised] := '‚ë§ë¢ ¥â ¨áª«îç¥­¨ï'; // DONE: translate
  FTranslation[trEnum] := '¥à¥ç¨á«¥­¨¥';
  FTranslation[trDescription] := '¯¨á ­¨¥';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := '®«ï';
  FTranslation[trFunctionsAndProcedures] := 'à®æ¥¤ãàë ¨ äã­ªæ¨¨';
  FTranslation[trHelp] := 'Help';
    // Untranslated to avoid Russian file name for css
  FTranslation[trHierarchy] := 'ˆ¥à àå¨ï';
  FTranslation[trIdentifiers] := 'ˆ¤¥­â¨ä¨ª â®àë';
  FTranslation[trInterface] := 'ˆ­â¥àä¥©á';
  FTranslation[trLegend] := '¡®§­ ç¥­¨ï';
  FTranslation[trMarker] := 'Œ àª¥à'; // DONE: translate
  FTranslation[trVisibility] := '‡®­  ¢¨¤¨¬®áâ¨'; // DONE: translate
  FTranslation[trLastModified] := '®á«¥¤­¥¥ ¨§¬¥­¥­¨¥';
  FTranslation[trMethods] := 'Œ¥â®¤ë';
  FTranslation[trName] := 'ˆ¬ï';
  FTranslation[trNone] := '¥â';
  FTranslation[trObject] := '¡ê¥ªâ';
  FTranslation[trObjects] := '¡ê¥ªâë';
  FTranslation[trOverview] := '¡§®à';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := '‘¢®©áâ¢ ';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := '’¨¯';
  FTranslation[trTypes] := '’¨¯ë';
  FTranslation[trUnit] := 'Œ®¤ã«ì';
  FTranslation[trUnits] := 'Œ®¤ã«¨';
  FTranslation[trVariables] := '¥à¥¬¥­­ë¥';
  FTranslation[trGvUses] := 'ƒà ä¨ª § ¢¨á¨¬®áâ¨ ¬®¤ã«¥©'; // DONE: translate
  FTranslation[trGvClasses] := 'ƒà ä¨ª ¨¥à àå¨¨ ª« áá®¢'; // DONE: translate
  FTranslation[trWarningOverwrite] :=
    'à¥¤ã¯à¥¦¤¥­¨¥: ­¥ à¥¤ ªâ¨à®¢ âì - íâ®â ä ©« á®§¤ ­  ¢â®¬ â¨ç¥áª¨ ¨ ¬®¦¥â ¡ëâì ¨§¬¥­ñ­ ¡¥§ ¯à¥¤ã¯à¥¦¤¥­¨ï';
  FTranslation[trWarning] := 'à¥¤ã¯à¥¦¤¥­¨¥';  // DONE: translate
  FTranslation[trHeadlineCio] := '‚á¥ ª« ááë, ¨­â¥àä¥©áë ¨ ®¡ê¥ªâë';
  FTranslation[trHeadlineConstants] := '‚á¥ ª®­áâ ­âë';
  FTranslation[trHeadlineFunctionsAndProcedures] := '‚á¥ ¯à®æ¥¤ãàë ¨ äã­ªæ¨¨';
  FTranslation[trHeadlineIdentifiers] := '‚á¥ ¨¤¥­â¨ä¨ª â®àë';
  FTranslation[trHeadlineTypes] := '‚á¥ â¨¯ë';
  FTranslation[trHeadlineUnits] := '‚á¥ ¬®¤ã«¨';
  FTranslation[trHeadlineVariables] := '‚á¥ ¯¥à¥¬¥­­ë¥';
  FTranslation[trSummaryCio] := '‘¯¨á®ª ª« áá®¢, ¨­â¥àä¥©á®¢ ¨ ®¡ê¥ªâ®¢';
  FTranslation[trGeneratedBy] := '‘£¥­¥à¨à®¢ « '; // DONE: translate
  FTranslation[trOnDateTime] := '¤ â /¢à¥¬ï'; // DONE: translate
  FTranslation[trDeprecated] := 'íâ®â á¨¬¢®« ¡®«ìè¥ ­¥ ¨á¯®«ì§ã¥âáï'; // DONE: translate
  FTranslation[trPlatformSpecific] := 'íâ®â á¨¬¢®« § ¢¨á¨â ®â ¯« âä®à¬ë'; // DONE: translate
  FTranslation[trLibrarySpecific] := 'íâ®â á¨¬¢®« § ¢¨á¨â ®â ¡¨¡«¨®â¥ª¨'; // DONE: translate
  FTranslation[trIntroduction] := '‚¢¥¤¥­¨¥'; // DONE: translate
  FTranslation[trConclusion] := '‡ ª«îç¥­¨¥'; // DONE: translate
  FTranslation[trSearch] := ' ©â¨'; // DONE: translate
  FTranslation[trSeeAlso] := 'Œ â¥à¨ «ë ¯® â¥¬¥'; // DONE: translate
  FTranslation[trValues] := '‡­ ç¥­¨¥'; // DONE: translate
  FTranslation[trNoCIOs] := 'Œ®¤ã«¨ ­¥ á®¤¥à¦ â ª« áá®¢, ¨­â¥àä¥©á®¢, ®¡ê¥ªâ®¢ ¨ § ¯¨á¥©.'; // DONE: translate
  FTranslation[trNoCIOsForHierarchy] := 'Œ®¤ã«¨ ­¥ á®¤¥à¦ â ª« áá®¢, ¨­â¥àä¥©á®¢ ¨ ®¡ê¥ªâ®¢.'; // DONE: translate
  FTranslation[trNoTypes] := 'Œ®¤ã«¨ ­¥ á®¤¥à¦ â â¨¯®¢.'; // DONE: translate
  FTranslation[trNoVariables] := 'Œ®¤ã«¨ ­¥ á®¤¥à¦ â ¯¥à¥¬¥­­ëå.'; // DONE: translate
  FTranslation[trNoConstants] := 'Œ®¤ã«¨ ­¥ á®¤¥à¦ â ª®­áâ ­â.'; // DONE: translate
  FTranslation[trNoFunctions] := 'Œ®¤ã«¨ ­¥ á®¤¥à¦ â äã­ªæ¨¨ ¨ ¯à®æ¥¤ãàë.'; // DONE: translate
  FTranslation[trNoIdentifiers] := 'Œ®¤ã«¨ ­¥ á®¤¥à¦ â ­¨ ®¤­®£® ¨¤¥­â¨ä¨ª â®à .'; // DONE: translate
  FTranslation[trProgram] := 'à®£à ¬¬ '; // DONE: translate
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageRussian_koi8;
begin
  FTranslation[trAuthor] := 'á×ÔÏÒ';
  FTranslation[trAuthors] := 'á×ÔÏÒÙ';
  FTranslation[trCio] := 'ëÌÁÓÓÙ, ÉÎÔÅÒÆÅÊÓÙ É ÏÂßÅËÔÙ';
  FTranslation[trClass] := 'ëÌÁÓÓ';
  FTranslation[trClasses] := 'ëÌÁÓÓÙ';
  FTranslation[trConstants] := 'ëÏÎÓÔÁÎÔÙ';
  FTranslation[trCreated] := 'óÏÚÄÁÎÏ';
  FTranslation[trDeclaration] := 'ïÂßÑ×ÌÅÎÉÑ';
  FTranslation[trParameters] := 'ğÁÒÁÍÅÔÒÙ'; // DONE: translate
  FTranslation[trReturns] := '÷ÏÚ×ÒÁİÁÅÍÙÅ ÚÎÁŞÅÎÉÑ'; // DONE: translate
  FTranslation[trExceptions] := 'éÓËÌÀŞÅÎÉÑ'; // DONE: translate
  FTranslation[trExceptionsRaised] := '÷ÙÚÙ×ÁÅÔ ÉÓËÌÀŞÅÎÉÑ'; // DONE: translate
  FTranslation[trEnum] := 'ğÅÒÅŞÉÓÌÅÎÉÅ';
  FTranslation[trDescription] := 'ïĞÉÓÁÎÉÅ';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'ğÏÌÑ';
  FTranslation[trFunctionsAndProcedures] := 'ğÒÏÃÅÄÕÒÙ É ÆÕÎËÃÉÉ';
  FTranslation[trHelp] := 'Help';
    // Untranslated to avoid Russian file name for css
  FTranslation[trHierarchy] := 'éÅÒÁÒÈÉÑ';
  FTranslation[trIdentifiers] := 'éÄÅÎÔÉÆÉËÁÔÏÒÙ';
  FTranslation[trInterface] := 'éÎÔÅÒÆÅÊÓ';
  FTranslation[trLegend] := 'ïÂÏÚÎÁŞÅÎÉÑ';
  FTranslation[trMarker] := 'íÁÒËÅÒ'; // DONE: translate
  FTranslation[trVisibility] := 'úÏÎÁ ×ÉÄÉÍÏÓÔÉ'; // DONE: translate
  FTranslation[trLastModified] := 'ğÏÓÌÅÄÎÅÅ ÉÚÍÅÎÅÎÉÅ';
  FTranslation[trMethods] := 'íÅÔÏÄÙ';
  FTranslation[trName] := 'éÍÑ';
  FTranslation[trNone] := 'îÅÔ';
  FTranslation[trObject] := 'ïÂßÅËÔ';
  FTranslation[trObjects] := 'ïÂßÅËÔÙ';
  FTranslation[trOverview] := 'ïÂÚÏÒ';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'ó×ÏÊÓÔ×Á';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'ôÉĞ';
  FTranslation[trTypes] := 'ôÉĞÙ';
  FTranslation[trUnit] := 'íÏÄÕÌØ';
  FTranslation[trUnits] := 'íÏÄÕÌÉ';
  FTranslation[trVariables] := 'ğÅÒÅÍÅÎÎÙÅ';
  FTranslation[trGvUses] := 'çÒÁÆÉË ÚÁ×ÉÓÉÍÏÓÔÉ ÍÏÄÕÌÅÊ'; // DONE: translate
  FTranslation[trGvClasses] := 'çÒÁÆÉË ÉÅÒÁÒÈÉÉ ËÌÁÓÓÏ×'; // DONE: translate
  FTranslation[trWarningOverwrite] :=
    'ğÒÅÄÕĞÒÅÖÄÅÎÉÅ: ÎÅ ÒÅÄÁËÔÉÒÏ×ÁÔØ - ÜÔÏÔ ÆÁÊÌ ÓÏÚÄÁÎ Á×ÔÏÍÁÔÉŞÅÓËÉ É ÍÏÖÅÔ ÂÙÔØ ÉÚÍÅÎ£Î ÂÅÚ ĞÒÅÄÕĞÒÅÖÄÅÎÉÑ';
  FTranslation[trWarning] := 'ğÒÅÄÕĞÒÅÖÄÅÎÉÅ';  // DONE: translate
  FTranslation[trHeadlineCio] := '÷ÓÅ ËÌÁÓÓÙ, ÉÎÔÅÒÆÅÊÓÙ É ÏÂßÅËÔÙ';
  FTranslation[trHeadlineConstants] := '÷ÓÅ ËÏÎÓÔÁÎÔÙ';
  FTranslation[trHeadlineFunctionsAndProcedures] := '÷ÓÅ ĞÒÏÃÅÄÕÒÙ É ÆÕÎËÃÉÉ';
  FTranslation[trHeadlineIdentifiers] := '÷ÓÅ ÉÄÅÎÔÉÆÉËÁÔÏÒÙ';
  FTranslation[trHeadlineTypes] := '÷ÓÅ ÔÉĞÙ';
  FTranslation[trHeadlineUnits] := '÷ÓÅ ÍÏÄÕÌÉ';
  FTranslation[trHeadlineVariables] := '÷ÓÅ ĞÅÒÅÍÅÎÎÙÅ';
  FTranslation[trSummaryCio] := 'óĞÉÓÏË ËÌÁÓÓÏ×, ÉÎÔÅÒÆÅÊÓÏ× É ÏÂßÅËÔÏ×';
  FTranslation[trGeneratedBy] := 'óÇÅÎÅÒÉÒÏ×ÁÌ '; // DONE: translate
  FTranslation[trOnDateTime] := 'ÄÁÔÁ/×ÒÅÍÑ'; // DONE: translate
  FTranslation[trDeprecated] := 'ÜÔÏÔ ÓÉÍ×ÏÌ ÂÏÌØÛÅ ÎÅ ÉÓĞÏÌØÚÕÅÔÓÑ'; // DONE: translate
  FTranslation[trPlatformSpecific] := 'ÜÔÏÔ ÓÉÍ×ÏÌ ÚÁ×ÉÓÉÔ ÏÔ ĞÌÁÔÆÏÒÍÙ'; // DONE: translate
  FTranslation[trLibrarySpecific] := 'ÜÔÏÔ ÓÉÍ×ÏÌ ÚÁ×ÉÓÉÔ ÏÔ ÂÉÂÌÉÏÔÅËÉ'; // DONE: translate
  FTranslation[trIntroduction] := '÷×ÅÄÅÎÉÅ'; // DONE: translate
  FTranslation[trConclusion] := 'úÁËÌÀŞÅÎÉÅ'; // DONE: translate
  FTranslation[trSearch] := 'îÁÊÔÉ'; // DONE: translate
  FTranslation[trSeeAlso] := 'íÁÔÅÒÉÁÌÙ ĞÏ ÔÅÍÅ'; // DONE: translate
  FTranslation[trValues] := 'úÎÁŞÅÎÉÅ'; // DONE: translate
  FTranslation[trNoCIOs] := 'íÏÄÕÌÉ ÎÅ ÓÏÄÅÒÖÁÔ ËÌÁÓÓÏ×, ÉÎÔÅÒÆÅÊÓÏ×, ÏÂßÅËÔÏ× É ÚÁĞÉÓÅÊ.'; // DONE: translate
  FTranslation[trNoCIOsForHierarchy] := 'íÏÄÕÌÉ ÎÅ ÓÏÄÅÒÖÁÔ ËÌÁÓÓÏ×, ÉÎÔÅÒÆÅÊÓÏ× É ÏÂßÅËÔÏ×.'; // DONE: translate
  FTranslation[trNoTypes] := 'íÏÄÕÌÉ ÎÅ ÓÏÄÅÒÖÁÔ ÔÉĞÏ×.'; // DONE: translate
  FTranslation[trNoVariables] := 'íÏÄÕÌÉ ÎÅ ÓÏÄÅÒÖÁÔ ĞÅÒÅÍÅÎÎÙÈ.'; // DONE: translate
  FTranslation[trNoConstants] := 'íÏÄÕÌÉ ÎÅ ÓÏÄÅÒÖÁÔ ËÏÎÓÔÁÎÔ.'; // DONE: translate
  FTranslation[trNoFunctions] := 'íÏÄÕÌÉ ÎÅ ÓÏÄÅÒÖÁÔ ÆÕÎËÃÉÉ É ĞÒÏÃÅÄÕÒÙ.'; // DONE: translate
  FTranslation[trNoIdentifiers] := 'íÏÄÕÌÉ ÎÅ ÓÏÄÅÒÖÁÔ ÎÉ ÏÄÎÏÇÏ ÉÄÅÎÔÉÆÉËÁÔÏÒÁ.'; // DONE: translate
  FTranslation[trProgram] := 'ğÒÏÇÒÁÍÍÁ'; // DONE: translate
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageSlovak;
begin
  FTranslation[trAuthor] := 'Autor';
  FTranslation[trAuthors] := 'Autori';
  FTranslation[trCio] := 'Triedy, interfejsy a objekty';
  FTranslation[trClass] := 'Trieda';
  FTranslation[trClasses] := 'Triedy';
  FTranslation[trConstants] := 'Konštanty';
  FTranslation[trCreated] := 'Vytvorené';
  FTranslation[trDeclaration] := 'Deklarácie';
  FTranslation[trDescription] := 'Popis';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions'; // TODO: translate
  FTranslation[trExceptionsRaised] := 'Exceptions raised'; // TODO: translate
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trFields] := 'Poloky';
  FTranslation[trFunctionsAndProcedures] := 'Funkcie a procedúry';
  FTranslation[trHierarchy] := 'Hierarchia';
  FTranslation[trIdentifiers] := 'Identifikátory';
  FTranslation[trInterface] := 'Interfejs';
  FTranslation[trLastModified] := 'Posledná zmena';
  FTranslation[trMethods] := 'Metódy';
  FTranslation[trName] := 'Meno';
  FTranslation[trNone] := 'Niè';
  FTranslation[trObject] := 'Objekt';
  FTranslation[trObjects] := 'Objekty';
  FTranslation[trOverview] := 'Overview';
  FTranslation[trProperties] := 'Monosti';
  FTranslation[trType] := 'Typ';
  FTranslation[trTypes] := 'Typy';
  FTranslation[trUnit] := 'Jednotka';
  FTranslation[trUnits] := 'Jednotky';
  FTranslation[trVariables] := 'Premenné';
  FTranslation[trGvUses] := 'Unit dependency graph'; // TODO: translate
  FTranslation[trGvClasses] := 'Classes hierarchy graph'; // TODO: translate
  FTranslation[trWarningOverwrite] :=
    'Upozornenie: Needitujte - tento súbor bol vytvorenı automaticky a je pravdepodobné, e bude prepísanı';
  FTranslation[trWarning] := 'Warning';  // TODO: translate
  FTranslation[trHeadlineCio] := 'Všetky triedy, interfejsy a objekty';
  FTranslation[trHeadlineConstants] := 'Všetky konštanty';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Všetky funkcie a procedúry';
  FTranslation[trHeadlineIdentifiers] := 'Všetky identifikátory';
  FTranslation[trHeadlineTypes] := 'Všetky typy';
  FTranslation[trHeadlineUnits] := 'Všetky jednotky';
  FTranslation[trHeadlineVariables] := 'Všetky premenné';
  FTranslation[trSummaryCio] := 'Zoznam tried, interfejsov a objektov';
  FTranslation[trGeneratedBy] := 'Generated by'; // TODO: translate
  FTranslation[trOnDateTime] := 'on'; // TODO: translate
  FTranslation[trDeprecated] := 'this symbol is deprecated';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library';
  FTranslation[trIntroduction] := 'Introduction'; // TODO: translate
  FTranslation[trConclusion] := 'Conclusion'; // TODO: translate
  FTranslation[trSearch] := 'Search'; // TODO: translate
  FTranslation[trSeeAlso] := 'See also'; // TODO: translate
  FTranslation[trValues] := 'Values'; // TODO: translate
  FTranslation[trNoCIOs] := 'The units do not contain any classes, interfaces, objects or records.'; // TODO: translate
  FTranslation[trNoCIOsForHierarchy] := 'The units do not contain any classes, interfaces or objects.'; // TODO: translate
  FTranslation[trNoTypes] := 'The units do not contain any types.'; // TODO: translate
  FTranslation[trNoVariables] := 'The units do not contain any variables.'; // TODO: translate
  FTranslation[trNoConstants] := 'The units do not contain any constants.'; // TODO: translate
  FTranslation[trNoFunctions] := 'The units do not contain any functions or procedures.'; // TODO: translate
  FTranslation[trNoIdentifiers] := 'The units do not contain any identifiers.'; // TODO: translate
  FTranslation[trProgram] := 'Program'; // TODO: translate
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageSpanish;
begin
  FTranslation[trAuthor] := 'Autor';
  FTranslation[trAuthors] := 'Autores';
  FTranslation[trAutomated] := 'Automated';
  FTranslation[trCio] := 'Clases, interfaces y objetos';
  FTranslation[trClass] := 'Clase';
  FTranslation[trClasses] := 'Clases';
  FTranslation[trClassHierarchy] := 'Jerarquía de clases';
  FTranslation[trConstants] := 'Constantes';
  FTranslation[trCreated] := 'Creado';
  FTranslation[trDeclaration] := 'Declaración';
  FTranslation[trDescription] := 'Descripción';
  FTranslation[trParameters] := 'Parámetros';
  FTranslation[trReturns] := 'Retornos';
  FTranslation[trExceptions] := 'Excepciones';
  FTranslation[trExceptionsRaised] := 'Excepciones lanzadas';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Campos';
  FTranslation[trFunctionsAndProcedures] := 'Funciones y procedimientos';
  FTranslation[trHelp] := 'Ayuda';
  FTranslation[trHierarchy] := 'Jerarquía';
  FTranslation[trIdentifiers] := 'Identificadores';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLegend] := 'Leyenda';
  FTranslation[trMarker] := 'Marcador';
  FTranslation[trVisibility] := 'Visibilidad';
  FTranslation[trMethods] := 'Métodos';
  FTranslation[trLastModified] := 'Última modificación';
  FTranslation[trName] := 'Nombre';
  FTranslation[trNone] := 'Ninguno';
  FTranslation[trObject] := 'Objeto';
  FTranslation[trObjects] := 'Objetos';
  FTranslation[trOverview] := 'Resumen';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Propiedades';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Tipo';
  FTranslation[trTypes] := 'Tipos';
  FTranslation[trUnit] := 'Unidad';
  FTranslation[trUnits] := 'Unidades';
  FTranslation[trVariables] := 'Variables';
  FTranslation[trGvUses] := 'Gráfico de las dependencias de unidades';
  FTranslation[trGvClasses] := 'Gráfico de la jerarquía de clases';
  FTranslation[trHeadlineCio] := 'Todas las clases, interfaces y objetos';
  FTranslation[trHeadlineConstants] := 'Todas las constantes';
  FTranslation[trHeadlineFunctionsAndProcedures] :=    'Todos las funciones y procedimientos';
  FTranslation[trHeadlineIdentifiers] := 'Todos los indentificadores';
  FTranslation[trHeadlineTypes] := 'Todos los tipos';
  FTranslation[trHeadlineUnits] := 'Todas las unidades';
  FTranslation[trHeadlineVariables] := 'Todas las variables';
  FTranslation[trSummaryCio] := 'Lista de clases, interfaces y objetos';
  FTranslation[trWarningOverwrite] :=  'Atención, no editar - este fichero ha sido creado automaticamente y puede ser sobrescrito';
  FTranslation[trWarning] := 'Warning';  // TODO: translate
  FTranslation[trGeneratedBy] := 'Generador por';
  FTranslation[trOnDateTime] := 'a';
  FTranslation[trDeprecated] := 'Este símbolo está obsoleto';
  FTranslation[trPlatformSpecific] := 'Este símbolo es específico para alguna plataforma';
  FTranslation[trLibrarySpecific] := 'Este símbolo es específico para alguna librería';
  FTranslation[trIntroduction] := 'Introducción';
  FTranslation[trConclusion] := 'Conclusión';
  FTranslation[trSearch] := 'Buscar';
  FTranslation[trSeeAlso] := 'Ver';
  FTranslation[trValues] := 'Valores';
  FTranslation[trNoCIOs] := 'Las unidades no contienen ni clases ni interfaces ni objetos ni registros.';
  FTranslation[trNoCIOsForHierarchy] := 'Las unidades no contienen ni clases ni interfaces ni objetos.';
  FTranslation[trNoTypes] := 'Las unidades no contienen ningún tipo.';
  FTranslation[trNoVariables] := 'Las unidades no contienen ningunas variables.';
  FTranslation[trNoConstants] := 'Las unidades no contienen ningunas constantes.';
  FTranslation[trNoFunctions] := 'Las unidades no contienen ni variables ni procedimientos.';
  FTranslation[trNoIdentifiers] := 'Las unidades no contienen ningún Identificador.';
  FTranslation[trProgram] := 'Program'; // TODO: translate
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageSwedish;
begin
  FTranslation[trAuthor] := 'Författare';
  FTranslation[trAuthors] := 'Författare';
  FTranslation[trCio] := 'Klasser, interface och objekt';
  FTranslation[trClass] := 'Klass';
  FTranslation[trClasses] := 'Klasser';
  FTranslation[trConstants] := 'Constants';
  FTranslation[trCreated] := 'Skapad';
  FTranslation[trDeclaration] := 'Deklarationer';
  FTranslation[trDescription] := 'Beskrivning';
  FTranslation[trParameters] := 'Se parameter';
  FTranslation[trReturns] := 'Retur';
  FTranslation[trExceptions] := 'Exceptions'; // TODO: translate
  FTranslation[trExceptionsRaised] := 'Exceptions raised'; // TODO: translate
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Fält';
  FTranslation[trFunctionsAndProcedures] := 'Functions and Procedures';
  FTranslation[trHelp] := 'Help';
    // Untranslated to avoid Swedish file name for css
  FTranslation[trHierarchy] := 'Hierarki';
  FTranslation[trIdentifiers] := 'Identifiers';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLegend] := 'Förklaring';
  FTranslation[trMarker] := 'Marker'; // TODO: translate
  FTranslation[trVisibility] := 'Visibility'; // TODO: translate
  FTranslation[trMethods] := 'Metoder';
  FTranslation[trLastModified] := 'Senast ändrad';
  FTranslation[trName] := 'Namn';
  FTranslation[trNone] := 'Ingen/inget.';
  FTranslation[trObject] := 'Objekt';
  FTranslation[trObjects] := 'Objekt';
  FTranslation[trOverview] := 'Översikt';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Properties';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Typer';
  FTranslation[trTypes] := 'Typer';
  FTranslation[trUnit] := 'Enhet';
  FTranslation[trUnits] := 'Enheter';
  FTranslation[trVariables] := 'Variabler';
  FTranslation[trGvUses] := 'Unit dependency graph'; // TODO: translate
  FTranslation[trGvClasses] := 'Classes hierarchy graph'; // TODO: translate
  FTranslation[trHeadlineCio] := 'Alla klasser, interface och objekt';
  FTranslation[trHeadlineConstants] := 'All Constants';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Alla funktioner och procedurer';
  FTranslation[trHeadlineIdentifiers] := 'Alla identifierare';
  FTranslation[trHeadlineTypes] := 'Alla typer';
  FTranslation[trHeadlineUnits] := 'Alla enheter';
  FTranslation[trHeadlineVariables] := 'Alla variabler';
  FTranslation[trSummaryCio] :=
    'Sammanfattning av Klasser, Interface, Objekt';
  FTranslation[trWarningOverwrite] :=
    'Varning: ändra inte denna fil manuellt - filen har skapats automatiskt och kommer troligen att skrivas över vid ett senare tilfälle';
  FTranslation[trWarning] := 'Warning';  // TODO: translate
  FTranslation[trGeneratedBy] := 'Generated by'; // TODO: translate
  FTranslation[trOnDateTime] := 'on'; // TODO: translate
  FTranslation[trDeprecated] := 'this symbol is deprecated'; // TODO: translate
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform'; // TODO: translate
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library'; // TODO: translate
  FTranslation[trIntroduction] := 'Introduction'; // TODO: translate
  FTranslation[trConclusion] := 'Conclusion'; // TODO: translate
  FTranslation[trSearch] := 'Search'; // TODO: translate
  FTranslation[trSeeAlso] := 'See also'; // TODO: translate
  FTranslation[trValues] := 'Values'; // TODO: translate
  FTranslation[trNoCIOs] := 'The units do not contain any classes, interfaces, objects or records.'; // TODO: translate
  FTranslation[trNoCIOsForHierarchy] := 'The units do not contain any classes, interfaces or objects.'; // TODO: translate
  FTranslation[trNoTypes] := 'The units do not contain any types.'; // TODO: translate
  FTranslation[trNoVariables] := 'The units do not contain any variables.'; // TODO: translate
  FTranslation[trNoConstants] := 'The units do not contain any constants.'; // TODO: translate
  FTranslation[trNoFunctions] := 'The units do not contain any functions or procedures.'; // TODO: translate
  FTranslation[trNoIdentifiers] := 'The units do not contain any identifiers.'; // TODO: translate
  FTranslation[trProgram] := 'Program'; // TODO: translate
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageHungarian_1250;
begin
  FTranslation[trAuthor] := 'Szerzõ';
  FTranslation[trAuthors] := 'Szerzõk';
  FTranslation[trAutomated] := 'Automatikus';
  FTranslation[trCio] := 'Osztályok, Kapcsolódási felületek és Objektumok';
  FTranslation[trClass] := 'Osztály';
  FTranslation[trClasses] := 'Osztályok';
  FTranslation[trClassHierarchy] := 'Osztály hierarchia';
  FTranslation[trConstants] := 'Konstansok';
  FTranslation[trCreated] := 'Készült';
  FTranslation[trDeclaration] := 'Deklaráció';
  FTranslation[trDescription] := 'Megjegyzés';
  FTranslation[trParameters] := 'Paraméterek';
  FTranslation[trReturns] := 'Visszatérési értékek';
  FTranslation[trExceptions] := 'Kivételek';
  FTranslation[trExceptionsRaised] := 'Kivételek kiemelése';
  FTranslation[trEnum] := 'Felsorolások';
  FTranslation[trDispInterface] := 'Képernyõ felületek';
  FTranslation[trFields] := 'Mezõk';
  FTranslation[trFunctionsAndProcedures] := 'Függvények és Eljárások';
  FTranslation[trHelp] := 'Súgó';
  FTranslation[trHierarchy] := 'Hierarchia';
  FTranslation[trIdentifiers] := 'Azonosítók';
  FTranslation[trInterface] := 'Kapcsolódási felület';
  FTranslation[trLegend] := 'Történet';
  FTranslation[trMarker] := 'Jelzõ';
  FTranslation[trVisibility] := 'Láthatóság';
  FTranslation[trMethods] := 'Metódusok';
  FTranslation[trLastModified] := 'Utolsó módosítás';
  FTranslation[trName] := 'Név';
  FTranslation[trNone] := 'Nincs';
  FTranslation[trObject] := 'Objektum';
  FTranslation[trObjects] := 'Objektumok';
  FTranslation[trOverview] := 'Áttekintés';
  FTranslation[trPrivate] := 'Privát';
  FTranslation[trProperties] := 'Tulajdonságok';
  FTranslation[trProtected] := 'Védett';
  FTranslation[trPublic] := 'Publikus';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Publikált';
  FTranslation[trType] := 'Típus';
  FTranslation[trTypes] := 'Típusok';
  FTranslation[trUnit] := 'Egység';
  FTranslation[trUnits] := 'Egységek';
  FTranslation[trVariables] := 'Változók';
  FTranslation[trGvUses] := 'Egység függõségi gráf';
  FTranslation[trGvClasses] := 'Osztály hierarchia gráf';
  FTranslation[trHeadlineCio] := 'Összes Osztály, Kapcsolódási felület és Objektumok';
  FTranslation[trHeadlineConstants] := 'Összes Kontans';
  FTranslation[trHeadlineFunctionsAndProcedures] := 'Összes Függvény és Eljárás';
  FTranslation[trHeadlineIdentifiers] := 'Összes Azonosító';
  FTranslation[trHeadlineTypes] := 'Összes Típus';
  FTranslation[trHeadlineUnits] := 'Összes Egység';
  FTranslation[trHeadlineVariables] := 'Összes Változó';
  FTranslation[trSummaryCio] := 'Öszefoglaló az Osztályokról, Kapcsoldási felületekrõl és Objektumokról';
  FTranslation[trWarningOverwrite] := 'Vigyázat: Nem szerkesztendõ file - ez a file automatikusan készült, valószínûleg felülírásra kerülne';
  FTranslation[trWarning] := 'Warning';  // TODO: translate
  FTranslation[trGeneratedBy] := 'Készítette';
  FTranslation[trOnDateTime] := ''; //none in Hungarian language
  FTranslation[trDeprecated] := 'ez az azonosító érték nélküli';
  FTranslation[trPlatformSpecific] := 'ez az azonosító szükséges némely platform számára';
  FTranslation[trLibrarySpecific] := 'ez az azonosító szükséges némely library számára';
  FTranslation[trIntroduction] := 'Bevezetõ';
  FTranslation[trConclusion] := 'Összefoglaló';
  FTranslation[trSearch] := 'Keresés';
  FTranslation[trSeeAlso] := 'Lásd még';
  FTranslation[trValues] := 'Értékek';
  FTranslation[trNoCIOs] := 'Az egység nem tartalmaz osztályt, interfészt, objektumot, vagy rekordot.';
  FTranslation[trNoCIOsForHierarchy] := 'Az egység nem tartalmaz osztályt, interfészt vagy objektumot.';
  FTranslation[trNoTypes] := 'Az egység nem tartalmaz típusokat';
  FTranslation[trNoVariables] := 'Az egység nem tartalmaz változókat.';
  FTranslation[trNoConstants] := 'Az egység nem tartalmaz konstansokat.';
  FTranslation[trNoFunctions] := 'Az egység nem tartalmaz függvényeket vagy eljárásokat.';
  FTranslation[trNoIdentifiers] := 'Az egység nem tartalmaz azonosítókat.';
  FTranslation[trProgram] := 'Program';
end;


type
  TLanguageRecord = record
    Table: PTransTable;
    Name: string;
    Syntax: string;
    CharSet: string;
  end;

const
  LANGUAGE_ARRAY: array[TLanguageID] of TLanguageRecord = (
    (Table: @aEnglish; Name: 'Default=English'; Syntax: 'en'; CharSet: 'iso-8859-1'),
    ( Name: 'Bosnian (Codepage 1250)'; Syntax: 'ba'; CharSet: 'windows-1250'),
    ( Name: 'Brasilian'; Syntax: 'br'; CharSet: ''),
    ( Name: 'Catalan'; Syntax: 'ct'; CharSet: ''),
    ( Name: 'Chinese (Codepage 950)'; Syntax: 'big5'; CharSet: 'big5'),
    ( Name: 'Chinese (Simple, gb2312)'; Syntax: 'gb2312'; CharSet: 'gb2312'),
    ( Name: 'Danish'; Syntax: 'dk'; CharSet: 'iso-8859-15'),
    ( Name: 'Dutch'; Syntax: 'nl'; CharSet: 'iso-8859-15'),
    (Table: @aEnglish; Name: 'English'; Syntax: 'en'; CharSet: 'iso-8859-1'),
    ( Name: 'French'; Syntax: 'fr'; CharSet: 'iso-8859-15'),
    (Table: @aGerman; Name: 'German'; Syntax: 'de'; CharSet: 'iso-8859-15'),
    ( Name: 'Indonesian'; Syntax: 'id'; CharSet: ''),
    ( Name: 'Italian'; Syntax: 'it'; CharSet: 'iso-8859-15'),
    ( Name: 'Javanese'; Syntax: 'jv'; CharSet: ''),
    ( Name: 'Polish (Codepage CP1250)'; Syntax: 'pl.cp1250'; CharSet: 'windows-1250'),
    ( Name: 'Polish (Codepage ISO 8859-2)'; Syntax: 'pl.iso-8859-2'; CharSet: 'iso-8859-2'),
    ( Name: 'Russian (Codepage 1251)'; Syntax: 'ru.1251'; CharSet: 'windows-1251'),
    ( Name: 'Russian (Codepage 866)'; Syntax: 'ru.866'; CharSet: 'IBM866'),
    ( Name: 'Russian (KOI-8)'; Syntax: 'ru.KOI8'; CharSet: 'koi8-r'),
    ( Name: 'Slovak'; Syntax: 'sk'; CharSet: ''),
    ( Name: 'Spanish'; Syntax: 'es'; CharSet: 'iso-8859-15'),
    ( Name: 'Swedish'; Syntax: 'se'; CharSet: 'iso-8859-15'),
    ( Name: 'Hungarian (Codepage 1250)'; Syntax: 'hu.1250'; CharSet: 'windows-1250')
  );

function TPasDocLanguages.GetTranslation(ATranslationID: TTranslationID): string;
begin
{$IFDEF old}
  Result := FTranslation[ATranslationID];
{$ELSE}
  Result := pTable^[ATranslationID];
  if Result <= strKeep then
    Result := aEnglish[ATranslationID];
{$ENDIF}
end;

procedure TPasDocLanguages.SetTranslation(id: TTranslationID;
  const into: string);
begin
  pTable^[id] := into;
end;

constructor TPasDocLanguages.Create;
begin
  inherited;
  SetLanguage(lgDefault);
end;

procedure TPasDocLanguages.SetLanguage(const Value: TLanguageID);
begin
  FLanguage := Value;
  FCharSet := LANGUAGE_ARRAY[Value].Charset;
  pTable := LANGUAGE_ARRAY[Value].Table;
  if assigned(pTable) then
    exit; //array already exists

  pTable := addr(aNewLanguage); //default, writeable
  case Value of
  {$IFDEF old}
    lgEnglish: SetLanguageEnglish;
    lgGerman: SetLanguageGerman;
  {$ELSE}
  //todo: create arrays from these methods
    lgBosnian: SetLanguageBosnian;
    lgBrasilian: SetLanguageBrasilian;
    lgCatalan: SetLanguageCatalan;
    lgChinese_950: SetLanguageChinese_950;
    lgChinese_gb2312: SetLanguageChinese_gb2312;
    lgDanish: SetLanguageDanish;
    lgDutch: SetLanguageDutch;
    lgFrench: SetLanguageFrench;
    lgIndonesian: SetLanguageIndonesian;
    lgItalian: SetLanguageItalian;
    lgJavanese: SetLanguageJavanese;
    lgPolish_CP1250: SetLanguagePolish_CP1250;
    lgPolish_ISO_8859_2: SetLanguagePolish_ISO_8859_2;
    lgRussian_1251: SetLanguageRussian_1251;
    lgRussian_866: SetLanguageRussian_866;
    lgRussian_koi8: SetLanguageRussian_koi8;
    lgSlovak: SetLanguageSlovak;
    lgSpanish: SetLanguageSpanish;
    lgSwedish: SetLanguageSwedish;
    lgHungarian_1250: SetLanguageHungarian_1250;
  {$ENDIF}
  end;
end;

//------------- language helpers -----------------

function LanguageFromIndex(i: integer): string;
begin
  Result := language_array[TLanguageID(i)].Name;
end;

function SyntaxFromIndex(i: integer): string;
var
  l: TLanguageID absolute i;
begin
  Result := Language_array[l].Syntax;
end;

function IDfromLanguage(const s: string): TLanguageID;
var
  i: TLanguageID;
begin
  for i := low(i) to high(i) do begin
    if (LANGUAGE_ARRAY[i].Name = s)
    or (LANGUAGE_ARRAY[i].Syntax = s) then begin
      Result := i;
      exit;
    end;
  end;
  Result := lgEnglish;
end;

{$IFDEF debug}
function TranslationNameFromId(id: TTranslationID): string;
begin
  Result := aTransIdNames[id];
end;
{$ELSE}
{$ENDIF}

function  Translation(id: TTranslationID; lang: TLanguageID): string;
var
  tbl: PTransTable;
begin
  tbl := LANGUAGE_ARRAY[lang].Table;
  if not assigned(tbl) then
    tbl := @aEnglish;
  Result := tbl^[id];
end;

function LanguageFromStr(S: string; out LanguageId: TLanguageID): boolean;
var
  I: TLanguageID;
begin
  S := LowerCase(S);
  for I := Low(LANGUAGE_ARRAY) to High(LANGUAGE_ARRAY) do
  begin
    if LowerCase(LANGUAGE_ARRAY[I].Syntax) = S then
    begin
      Result := true;
      LanguageId := I;
      Exit;
    end;
  end;

  Result := false;
end;

end.
