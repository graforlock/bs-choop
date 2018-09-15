
/* Reason for below is that ML type systems don't allow
 for neither mixing different types in objects/lists,
 nor untagged union types which is required as a main
 characteristic for DOM API element interfacing.
*/
let toJSObj = [%raw {|
  function(l) {
    return l.reduce((acc, x) => {
      const [k, v] = x;
      acc[k] = v;
      return acc;
    }, {});
  }
|}];

module Emitter = {
  type t;

  type emit('a) = (. string, 'a) => unit;

  [@bs.send] external emit: t => string => 'a => unit = "emit";
  [@bs.send] external on: t => string => ('a => unit) => unit = "on"; 
};

module Html =  {
  type t;

  type xmlAttributes =
    | BoolAttr(string, bool)
    | StringAttr(string, string)
    | ObjectAttr(string, {. "__html": string })
    | FunctionAttr(string, unit => unit);
    
  type xmlElement('a) = (string, array(xmlAttributes));
  
  type xmlNode =
    | ParentNode  (string, array(xmlAttributes), list(xmlNode))
    | VoidElement (string, array(xmlAttributes))
    | EncodedText (string);

  [@bs.module] external h_: 'a => 'b => 'c => 'd = "choop/h";
 
  let h = fun
    | ParentNode(tag, attrs, children) => h_(tag, attrs |> toJSObj, Array.of_list(children))
    | VoidElement(tag, attrs) => h_(tag, attrs |> toJSObj, Js.Nullable.undefined)
    | EncodedText(content) => h_("", Js.Nullable.null, content);
  
  let attr = (key : string, value : string) => StringAttr(key, value);
  let func = (key : string, value : unit => unit) => FunctionAttr(key, value);
  let obj  = (key : string, value : {. "__html": string }) => ObjectAttr(key, value);
  let flag = (key : string) => BoolAttr(key, true);
  
  let tag = (nodeName   : string
          , attributes : list(xmlAttributes)
          , children   : list(xmlNode)) =>
      ParentNode (nodeName, Array.of_list(attributes), children) |> h;
  
  let voidTag = (nodeName  : string
              , attributes : list(xmlAttributes)) =>
      VoidElement (nodeName, Array.of_list(attributes)) |> h;
  
  let text        = (content : string) => EncodedText (content);
  let emptyText   = ()                 => text ("");
  
  let html       = tag ("html")

  /* let ``base``   = voidTag ("base") */
  let head       = tag ("head");
  let link = attr => voidTag ("link", attr);
  let meta = attr => voidTag ("meta", attr);
  let style      = tag ("style");
  let title      = tag ("title");
  
  let blockquote = tag ("blockquote");
  let body       = tag ("body");
  let address    = tag ("address");
  let article    = tag ("article");
  let aside      = tag ("aside");
  let footer     = tag ("footer");
  let hgroup     = tag ("hgroup");
  let h1         = tag ("h1");
  let h2         = tag ("h2");
  let h3         = tag ("h3");
  let h4         = tag ("h4");
  let h5         = tag ("h5");
  let h6         = tag ("h6");
  let header     = tag ("header");
  let nav        = tag ("nav");
  let section    = tag ("section");
  
  let dd         = tag ("dd");
  let div        = tag ("div");
  let dl         = tag ("dl");
  let dt         = tag ("dt");
  let figcaption = tag ("figcaption");
  let figure     = tag ("figure");
  let hr         = voidTag ("hr");
  let li         = tag ("li");
  let main       = tag ("main");
  let ol         = tag ("ol");
  let p          = tag ("p");
  let pre        = tag ("pre");
  let ul         = tag ("ul");
  
  let a          = tag ("a");
  let abbr       = tag ("abbr");
  let b          = tag ("b");
  let bdi        = tag ("bdi");
  let bdo        = tag ("bdo");
  let br         = voidTag ("br");
  let cite       = tag ("cite");
  let code       = tag ("code");
  let data       = tag ("data");
  let dfn        = tag ("dfn");
  let em         = tag ("em");
  let i          = tag ("i");
  let kbd        = tag ("kbd");
  let mark       = tag ("mark");
  let q          = tag ("q");
  let rp         = tag ("rp");
  let rt         = tag ("rt");
  let rtc        = tag ("rtc");
  let ruby       = tag ("ruby");
  let s          = tag ("s");
  let samp       = tag ("samp");
  let small      = tag ("small");
  let span       = tag ("span");
  let strong     = tag ("strong");
  let sub        = tag ("sub");
  let sup        = tag ("sup");
  let time       = tag ("time");
  let u          = tag ("u");
  let var        = tag ("var");
  let wbr        = voidTag ("wbr");
  
  let area       = voidTag ("area");
  let audio      = tag ("audio");
  let img        = voidTag ("img");
  let map        = tag ("map");
  let track      = voidTag ("track");
  let video      = tag ("video");
  
  let embed      = voidTag ("embed");
  let object_     = tag ("object");
  let param      = voidTag ("param");
  let source     = voidTag ("source");
  
  let canvas     = tag ("canvas");
  let noscript   = tag ("noscript");
  let script     = tag ("script");
  
  let del        = tag ("del");
  let ins        = tag ("ins");
  
  let caption    = tag ("caption");
  let col        = voidTag ("col");
  let colgroup   = tag ("colgroup");
  let table      = tag ("table");
  let tbody      = tag ("tbody");
  let td         = tag ("td");
  let tfoot      = tag ("tfoot");
  let th         = tag ("th");
  let thead      = tag ("thead");
  let tr         = tag ("tr");
  
  let button     = tag ("button");
  let datalist   = tag ("datalist");
  let fieldset   = tag ("fieldset");
  let form       = tag ("form");
  let input      = voidTag ("input");
  let label      = tag ("label");
  let legend     = tag ("legend");
  let meter      = tag ("meter");
  let optgroup   = tag ("optgroup");
  let option     = tag ("option");
  let output     = tag ("output");
  let progress   = tag ("progress");
  let select     = tag ("select");
  let textarea   = tag ("textarea");
  
  let details    = tag ("details");
  let dialog     = tag ("dialog");
  let menu       = tag ("menu");
  let menuitem   = voidTag ("menuitem");
  let summary    = tag ("summary");

  module Attributes = {
      let _dangerouslySetInnerHTML = obj ("dangerouslySetInnerHTML");

      let _abbr               = attr ("abbr");
      let _accept             = attr ("accept");
      let _acceptCharset      = attr ("accept-charset");
      let _accesskey          = attr ("accesskey");
      let _action             = attr ("action");
      let _alt                = attr ("alt");
      let _autocomplete       = attr ("autocomplete");
      let _border             = attr ("border");
      let _challenge          = attr ("challenge");
      let _charset            = attr ("charset");
      let _cite               = attr ("cite");
      let _class              = attr ("class");
      let _cols               = attr ("cols");
      let _colspan            = attr ("colspan");
      let _content            = attr ("content");
      let _contenteditable    = attr ("contenteditable");
      let _coords             = attr ("coords");
      let _crossorigin        = attr ("crossorigin");
      let _data               = attr ("data");
      let _datetime           = attr ("datetime");
      let _dir                = attr ("dir");
      let _dirname            = attr ("dirname");
      let _download           = attr ("download");
      let _enctype            = attr ("enctype");
      let _for                = attr ("for");
      let _form               = attr ("form");
      let _formaction         = attr ("formaction");
      let _formenctype        = attr ("formenctype");
      let _formmethod         = attr ("formmethod");
      let _formtarget         = attr ("formtarget");
      let _headers            = attr ("headers");
      let _height             = attr ("height");
      let _high               = attr ("high");
      let _href               = attr ("href");
      let _hreflang           = attr ("hreflang");
      let _httpEquiv          = attr ("http-equiv");
      let _id                 = attr ("id");
      let _integrity          = attr ("integrity");
      let _keytype            = attr ("keytype");
      let _kind               = attr ("kind");
      let _label              = attr ("label");
      let _lang               = attr ("lang");
      let _list               = attr ("list");
      let _low                = attr ("low");
      let _manifest           = attr ("manifest");
      let _max                = attr ("max");
      let _maxlength          = attr ("maxlength");
      let _media              = attr ("media");
      let _mediagroup         = attr ("mediagroup");
      let _method             = attr ("method");
      let _min                = attr ("min");
      let _minlength          = attr ("minlength");
      let _name               = attr ("name");
      let _optimum            = attr ("optimum");
      let _pattern            = attr ("pattern");
      let _placeholder        = attr ("placeholder");
      let _poster             = attr ("poster");
      let _preload            = attr ("preload");
      let _rel                = attr ("rel");
      let _rows               = attr ("rows");
      let _rowspan            = attr ("rowspan");
      let _sandbox            = attr ("sandbox");
      let _spellcheck         = attr ("spellcheck");
      let _scope              = attr ("scope");
      let _shape              = attr ("shape");
      let _size               = attr ("size");
      let _sizes              = attr ("sizes");
      let _span               = attr ("span");
      let _src                = attr ("src");
      let _srcdoc             = attr ("srcdoc");
      let _srclang            = attr ("srclang");
      let _start              = attr ("start");
      let _step               = attr ("step");
      let _style              = attr ("style");
      let _tabindex           = attr ("tabindex");
      let _target             = attr ("target");
      let _title              = attr ("title");
      let _translate          = attr ("translate");
      let _type               = attr ("type");
      let _usemap             = attr ("usemap");
      let _value              = attr ("value");
      let _width              = attr ("width");
      let _wrap               = attr ("wrap");
  
      let _onclick            = func ("onclick");
      let _oncontextmenu      = func ("oncontextmenu");
      let _ondblclick         = func ("ondblclick");
      let _onmousedown        = func ("onmousedown");
      let _onmouseenter       = func ("onmouseenter");
      let _onmouseleave       = func ("onmouseleave");
      let _onmousemove        = func ("onmousemove");
      let _onmouseout         = func ("onmouseout");
      let _onmouseover        = func ("onmouseover");
      let _onmouseup          = func ("onmouseup");
  
      let _ontouchcancel      = func ("ontouchcancel");
      let _ontouchend         = func ("ontouchend");
      let _ontouchmove        = func ("ontouchmove");
      let _ontouchstart       = func ("ontouchstart");
  
      let _onkeydown          = func ("onkeydown");
      let _onkeypress         = func ("onkeypress");
      let _onkeyup            = func ("onkeyup");
  
      let _ondrag             = func ("ondrag");
      let _ondragend          = func ("ondragend");
      let _ondragenter        = func ("ondragenter");
      let _ondragleave        = func ("ondragleave");
      let _ondragover         = func ("ondragover");
      let _ondragstart        = func ("ondragstart");
      let _ondrop             = func ("ondrop");
  
      let _onblur              = func ("onblur");
      let _onfocus             = func ("onfocus");
      let _onfocusin           = func ("onfocusin");
      let _onfocusout          = func ("onfocusout");
  
      let _oninput             = func ("oninput");
  
      let _onwheel            = func ("onwheel");
  
      let _async              = flag("async");
      let _autofocus          = flag ("autofocus");
      let _autoplay           = flag ("autoplay");
      let _checked            = flag ("checked");
      let _controls           = flag ("controls");
      let _default            = flag ("default");
      let _defer              = flag ("defer");
      let _disabled           = flag ("disabled");
      let _formnovalidate     = flag ("formnovalidate");
      let _hidden             = flag ("hidden");
      let _ismap              = flag ("ismap");
      let _loop               = flag ("loop");
      let _multiple           = flag ("multiple");
      let _muted              = flag ("muted");
      let _novalidate         = flag ("novalidate");
      let _readonly           = flag ("readonly");
      let _required           = flag ("required");
      let _reversed           = flag ("reversed");
      let _scoped             = flag ("scoped");
      let _selected           = flag ("selected");
      let _typemustmatch      = flag ("typemustmatch");
  
  };
  
  module Accessibility = {
      
      let _roleAlert            = attr ("role", "alert");
      let _roleAlertDialog      = attr ("role", "alertdialog");
      let _roleApplication      = attr ("role", "application");
      let _roleArticle          = attr ("role", "article");
      let _roleBanner           = attr ("role", "banner");
      let _roleButton           = attr ("role", "button");
      let _roleCell             = attr ("role", "cell");
      let _roleCheckBox         = attr ("role", "checkbox");
      let _roleColumnHeader     = attr ("role", "columnheader");
      let _roleComboBox         = attr ("role", "combobox");
      let _roleComplementary    = attr ("role", "complementary");
      let _roleContentInfo      = attr ("role", "contentinfo");
      let _roleDefinition       = attr ("role", "definition");
      let _roleDialog           = attr ("role", "dialog");
      let _roleDirectory        = attr ("role", "directory");
      let _roleDocument         = attr ("role", "document");
      let _roleFeed             = attr ("role", "feed");
      let _roleFigure           = attr ("role", "figure");
      let _roleForm             = attr ("role", "form");
      let _roleGrid             = attr ("role", "grid");
      let _roleGridCell         = attr ("role", "gridcell");
      let _roleGroup            = attr ("role", "group");
      let _roleHeading          = attr ("role", "heading");
      let _roleImg              = attr ("role", "img");
      let _roleLink             = attr ("role", "link");
      let _roleList             = attr ("role", "list");
      let _roleListBox          = attr ("role", "listbox");
      let _roleListItem         = attr ("role", "listitem");
      let _roleLog              = attr ("role", "log");
      let _roleMain             = attr ("role", "main");
      let _roleMarquee          = attr ("role", "marquee");
      let _roleMath             = attr ("role", "math");
      let _roleMenuBar          = attr ("role", "menubar");
      let _roleMenuItem         = attr ("role", "menuitem");
      let _roleMenuItemCheckBox = attr ("role", "menuitemcheckbox");
      let _roleMenuItemRadio    = attr ("role", "menuitemradio");
      let _roleNavigation       = attr ("role", "navigation");
      let _roleNone             = attr ("role", "none");
      let _roleNote             = attr ("role", "note");
      let _roleOption           = attr ("role", "option");
      let _rolePresentation     = attr ("role", "presentation");
      let _roleProgressBar      = attr ("role", "progressbar");
      let _roleRadio            = attr ("role", "radio");
      let _roleRadioGroup       = attr ("role", "radiogroup");
      let _roleRegion           = attr ("role", "region");
      let _roleRow              = attr ("role", "row");
      let _roleRowGroup         = attr ("role", "rowgroup");
      let _roleRowHeader        = attr ("role", "rowheader");
      let _roleScrollBar        = attr ("role", "scrollbar");
      let _roleSearch           = attr ("role", "search");
      let _roleSearchBox        = attr ("role", "searchbox");
      let _roleSeparator        = attr ("role", "separator");
      let _roleSlider           = attr ("role", "slider");
      let _roleSpinButton       = attr ("role", "spinbutton");
      let _roleStatus           = attr ("role", "status");
      let _roleSwitch           = attr ("role", "switch");
      let _roleTab              = attr ("role", "tab");
      let _roleTable            = attr ("role", "table");
      let _roleTabList          = attr ("role", "tablist");
      let _roleTabPanel         = attr ("role", "tabpanel");
      let _roleTerm             = attr ("role", "term");
      let _roleTextBox          = attr ("role", "textbox");
      let _roleTimer            = attr ("role", "timer");
      let _roleToolBar          = attr ("role", "toolbar");
      let _roleToolTip          = attr ("role", "tooltip");
      let _roleTree             = attr ("role", "tree");
      let _roleTreeGrid         = attr ("role", "treegrid");
      let _roleTreeItem         = attr ("role", "treeitem");
      
      let _ariaActiveDescendant = attr ("aria-activedescendant");
      let _ariaAtomic           = attr ("aria-atomic");
      let _ariaAutocomplete     = attr ("aria-autocomplete");
      let _ariaBusy             = attr ("aria-busy");
      let _ariaChecked          = attr ("aria-checked");
      let _ariaColCount         = attr ("aria-colcount");
      let _ariaColIndex         = attr ("aria-colindex");
      let _ariaColSpan          = attr ("aria-colspan");
      let _ariaControls         = attr ("aria-controls");
      let _ariaCurrent          = attr ("aria-current");
      let _ariaDescribedBy      = attr ("aria-describedby");
      let _ariaDetails          = attr ("aria-details");
      let _ariaDisabled         = attr ("aria-disabled");
      let _ariaDropEffect       = attr ("aria-dropeffect");
      let _ariaErrorMessage     = attr ("aria-errormessage");
      let _ariaExpanded         = attr ("aria-expanded");
      let _ariaFlowTo           = attr ("aria-flowto");
      let _ariaGrabbed          = attr ("aria-grabbed");
      let _ariaHasPopup         = attr ("aria-haspopup");
      let _ariaHidden           = attr ("aria-hidden");
      let _ariaInvalid          = attr ("aria-invalid");
      let _ariaKeyShortcuts     = attr ("aria-keyshortcuts");
      let _ariaLabel            = attr ("aria-label");
      let _ariaLabelledBy       = attr ("aria-labeledby");
      let _ariaLevel            = attr ("aria-level");
      let _ariaLive             = attr ("aria-live");
      let _ariaModal            = attr ("aria-modal");
      let _ariaMultiline        = attr ("aria-multiline");
      let _ariaMultiSelectable  = attr ("aria-multiselectable");
      let _ariaOrientation      = attr ("aria-orientation");
      let _ariaOwns             = attr ("aria-owns");
      let _ariaPlaceholder      = attr ("aria-placeholder");
      let _ariaPosInset         = attr ("aria-posinset");
      let _ariaPressed          = attr ("aria-pressed");
      let _ariaReadOnly         = attr ("aria-readonly");
      let _ariaRelevant         = attr ("aria-relevant");
      let _ariaRequired         = attr ("aria-required");
      let _ariaRoleDescription  = attr ("aria-roledescription");
      let _ariaRowCount         = attr ("aria-rowcount");
      let _ariaRowIndex         = attr ("aria-rowindex");
      let _ariaRowSpan          = attr ("aria-rowspan");
      let _ariaSelected         = attr ("aria-selected");
      let _ariaSetSize          = attr ("aria-setsize");
      let _ariaSort             = attr ("aria-sort");
      let _ariaValueMax         = attr ("aria-valuemax");
      let _ariaValueMin         = attr ("aria-valuemin");
      let _ariaValueNow         = attr ("aria-valuenow");
      let _ariaValueText        = attr ("aria-valuetext");
  };
};

module App = {
  type t;

  type render('a, 'b) = 'a => Emitter.emit('b) => Html.t;

  [@bs.module] external make: unit => t = "choop";
  [@bs.send] external use: t => (('a, Emitter.t) => unit) => unit = "use";
  [@bs.send] external route: t => string => render('a, 'b) => unit = "route";
  [@bs.send] external mount: t => string => unit = "mount";
};