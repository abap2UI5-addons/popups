"! Developer search popup for data elements: searches the texts of DD04T
"! (description, short, medium, long and heading text) and the name of the
"! data element in a chosen language and shows a second language side by
"! side. The caller reads the chosen data element with result( ).
"! Embedded (z2ui5_if_popup_embed) it is a search page inside a host app.
"!
"! The DDIC tables are reached by name only (dynamic FROM), so the class also
"! compiles on ABAP Cloud, where they are not released - there the popup shows
"! that the search is not available.
CLASS z2ui5_cl_popup_dtel_srch DEFINITION PUBLIC.

  PUBLIC SECTION.
    " abap2ui5lint-disable unbound-public-attribute -- a popup's attributes are its interface to the app that called it, which reads them once the popup returns
    INTERFACES z2ui5_if_app.
    " check_confirmed( ) - a data element chosen; value( ) - REF TO
    " ty_s_result, the chosen data element with its texts (ms_result);
    " value_string( ) - the name of the data element
    INTERFACES z2ui5_if_popup_result.
    " embedded mode: with mo_parent_view set by the host, the search renders
    " into the host's page - no popup, no OK/Cancel, no nav_app_leave( )
    INTERFACES z2ui5_if_popup_embed.

    TYPES:
      BEGIN OF ty_s_result,
        rollname        TYPE string,
        datatype        TYPE string,
        leng            TYPE string,
        description     TYPE string,
        short           TYPE string,
        medium          TYPE string,
        long            TYPE string,
        heading         TYPE string,
        check_confirmed TYPE abap_bool,
      END OF ty_s_result.

    TYPES:
      BEGIN OF ty_s_row,
        selkz        TYPE abap_bool,
        rollname     TYPE c LENGTH 30,
        datatype     TYPE string,
        leng         TYPE string,
        count        TYPE i,
        description  TYPE string,
        short        TYPE string,
        medium       TYPE string,
        long         TYPE string,
        heading      TYPE string,
        description2 TYPE string,
        long2        TYPE string,
      END OF ty_s_row.
    TYPES ty_t_row TYPE STANDARD TABLE OF ty_s_row WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_langu,
        key  TYPE string,
        text TYPE string,
      END OF ty_s_langu.
    TYPES ty_t_langu TYPE STANDARD TABLE OF ty_s_langu WITH DEFAULT KEY.

    DATA ms_result         TYPE ty_s_result.
    DATA mv_search         TYPE string.
    DATA mv_langu          TYPE string.
    DATA mv_langu2         TYPE string.
    DATA mt_langu          TYPE ty_t_langu.
    DATA mt_result         TYPE ty_t_row.
    " hits translated into fewer languages are left out; 0 shows all.
    " Public so a host that creates the app itself (CREATE OBJECT, embedded
    " mode) can set it as factory( i_min_languages ) does
    DATA mv_min_languages  TYPE i.
    DATA mv_check_disabled TYPE abap_bool.
    DATA mv_info           TYPE string.
    DATA mv_check_info     TYPE abap_bool.

    "! @parameter i_search | prefilled search text; when supplied, the
    "!                       search runs as soon as the popup opens
    "! @parameter i_langu | language of the texts, default English
    "! @parameter i_langu2 | second language shown side by side, default
    "!                       German (English when i_langu is German)
    "! @parameter i_max_rows | maximum number of data elements read
    "! @parameter i_min_languages | only hits with texts in at least this many
    "!                              languages (count of the Languages column); 0 = all
    CLASS-METHODS factory
      IMPORTING
        i_search          TYPE clike     OPTIONAL
        i_langu           TYPE sy-langu  DEFAULT 'E'
        i_langu2          TYPE sy-langu  OPTIONAL
        i_title           TYPE clike     DEFAULT `Data Element Search`
        i_max_rows        TYPE i         DEFAULT 200
        i_min_languages   TYPE i         DEFAULT 0
        i_event_canceled  TYPE string    OPTIONAL
        i_event_confirmed TYPE string    OPTIONAL
          PREFERRED PARAMETER i_search
      RETURNING
        VALUE(r_result)   TYPE REF TO z2ui5_cl_popup_dtel_srch.

    METHODS result
      RETURNING
        VALUE(result) TYPE ty_s_result.
    " abap2ui5lint-enable unbound-public-attribute

  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_s_t002t,
        sprsl TYPE c LENGTH 1,
        sptxt TYPE c LENGTH 16,
      END OF ty_s_t002t.
    TYPES ty_t_t002t TYPE STANDARD TABLE OF ty_s_t002t WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_dd04t,
        rollname   TYPE c LENGTH 30,
        ddlanguage TYPE c LENGTH 1,
        ddtext     TYPE c LENGTH 60,
        reptext    TYPE c LENGTH 55,
        scrtext_s  TYPE c LENGTH 10,
        scrtext_m  TYPE c LENGTH 20,
        scrtext_l  TYPE c LENGTH 40,
      END OF ty_s_dd04t.
    TYPES ty_t_dd04t TYPE STANDARD TABLE OF ty_s_dd04t WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_dd04l,
        rollname TYPE c LENGTH 30,
        datatype TYPE c LENGTH 4,
        leng     TYPE n LENGTH 6,
      END OF ty_s_dd04l.
    TYPES ty_t_dd04l TYPE STANDARD TABLE OF ty_s_dd04l WITH DEFAULT KEY.

    DATA client          TYPE REF TO z2ui5_if_client.
    DATA title           TYPE string.
    DATA max_rows        TYPE i.
    DATA event_confirmed TYPE string.
    DATA event_canceled  TYPE string.
    "! embedded mode: on_init( ) ran - check_on_init( ) is the host's
    DATA embed_initialized TYPE abap_bool.

    METHODS on_init.
    "! main( ) in embedded mode - see z2ui5_if_popup_embed
    METHODS main_embedded.
    METHODS on_event.
    METHODS on_event_confirm.
    METHODS display.
    METHODS search.
    METHODS read_languages.

    "! leave out the hits with fewer languages than mv_min_languages
    METHODS apply_min_languages.

    "! The languages of the two Selects: English and German first, then the
    "! other languages alphabetically - those installed (T002C-LAINST), the
    "! logon language and the selected ones; all of T002T when installed is
    "! empty (T002C not readable)
    "! @parameter t002t | key and text of the languages, in the logon language
    "! @parameter installed | the installed languages, empty for no filter
    METHODS get_language_list
      IMPORTING
        t002t         TYPE ty_t_t002t
        installed     TYPE string_table
      RETURNING
        VALUE(result) TYPE ty_t_langu.
    METHODS read_details.

    "! A LIKE pattern for the value: the escape character #, % and _ are
    "! escaped so that they match only themselves, * becomes the wildcard %.
    "! The pattern is passed as a host variable, never as part of a dynamic
    "! WHERE, so a quote cannot end a literal.
    METHODS get_like_pattern
      IMPORTING
        val           TYPE string
        contains      TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result) TYPE string.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_dtel_srch IMPLEMENTATION.

  METHOD factory.
    DATA temp51 TYPE string.
    DATA temp52 TYPE i.

    CREATE OBJECT r_result.
    r_result->title           = i_title.
    r_result->mv_search       = i_search.
    r_result->mv_langu        = i_langu.

    IF i_langu2 IS NOT INITIAL.
      temp51 = i_langu2.
    ELSEIF i_langu = `D`.
      temp51 = `E`.
    ELSE.
      temp51 = `D`.
    ENDIF.
    r_result->mv_langu2       = temp51.

    IF i_max_rows > 0.
      temp52 = i_max_rows.
    ELSE.
      temp52 = 200.
    ENDIF.
    r_result->max_rows        = temp52.
    r_result->mv_min_languages = i_min_languages.
    r_result->event_canceled  = i_event_canceled.
    r_result->event_confirmed = i_event_confirmed.

  ENDMETHOD.

  METHOD result.

    result = ms_result.

  ENDMETHOD.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF z2ui5_if_popup_embed~mo_parent_view IS BOUND.
      main_embedded( ).
      RETURN.
    ENDIF.

    IF client->check_on_init( ) IS NOT INITIAL.
      on_init( ).
      display( ).
      RETURN.
    ENDIF.

    on_event( ).

  ENDMETHOD.

  METHOD main_embedded.

    IF embed_initialized = abap_false.
      embed_initialized = abap_true.
      on_init( ).
      z2ui5_if_popup_embed~mv_view_display = abap_true.
    ELSEIF client->get( )-event = `SEARCH`.
      " the only event of its own - OK and Cancel do not exist here, every
      " other event belongs to the host
      search( ).
    ENDIF.

    " the host builds its page anew in every roundtrip: the content is
    " appended each time, the host displays it when mv_view_display is set
    display( ).

  ENDMETHOD.

  METHOD on_init.
      DATA temp53 TYPE string.

    IF z2ui5_cl_popup_context=>check_abap_cloud( ) IS NOT INITIAL.
      mv_check_disabled = abap_true.
      mv_info = `The data element search is not available on ABAP Cloud: DD04T is not released there.`.
      mv_check_info = abap_true.
      RETURN.
    ENDIF.

    " started without factory( ) (CREATE OBJECT from customizing): the
    " defaults of factory( )
    IF mv_langu IS INITIAL.
      mv_langu = `E`.
    ENDIF.
    IF mv_langu2 IS INITIAL.

      IF mv_langu = `D`.
        temp53 = `E`.
      ELSE.
        temp53 = `D`.
      ENDIF.
      mv_langu2 = temp53.
    ENDIF.
    IF max_rows <= 0.
      max_rows = 200.
    ENDIF.
    IF title IS INITIAL.
      title = `Data Element Search`.
    ENDIF.

    read_languages( ).

    IF mv_search IS NOT INITIAL.
      search( ).
    ENDIF.

  ENDMETHOD.

  METHOD on_event.

    CASE client->get( )-event.

      WHEN `SEARCH`.
        search( ).

      WHEN `CONFIRM`.
        on_event_confirm( ).

      WHEN `CANCEL`.
        ms_result-check_confirmed = abap_false.
        client->popup_destroy( ).
        client->nav_app_leave( event = event_canceled ).

    ENDCASE.

  ENDMETHOD.

  METHOD on_event_confirm.

    FIELD-SYMBOLS <ls_row> TYPE z2ui5_cl_popup_dtel_srch=>ty_s_row.
    READ TABLE mt_result WITH KEY selkz = abap_true ASSIGNING <ls_row>.
    IF <ls_row> IS NOT ASSIGNED.
      client->message_toast_display( `Select a data element first.` ).
      RETURN.
    ENDIF.

    CLEAR ms_result.
    ms_result-rollname = <ls_row>-rollname.
    ms_result-datatype = <ls_row>-datatype.
    ms_result-leng = <ls_row>-leng.
    ms_result-description = <ls_row>-description.
    ms_result-short = <ls_row>-short.
    ms_result-medium = <ls_row>-medium.
    ms_result-long = <ls_row>-long.
    ms_result-heading = <ls_row>-heading.
    ms_result-check_confirmed = abap_true.

    client->popup_destroy( ).
    client->nav_app_leave( event = event_confirmed ).

  ENDMETHOD.

  METHOD apply_min_languages.

    IF mv_min_languages <= 0.
      RETURN.
    ENDIF.
    DELETE mt_result WHERE count < mv_min_languages.
    IF mt_result IS INITIAL.
      client->message_toast_display( |No data elements with texts in at least { mv_min_languages } languages found.| ).
    ENDIF.

  ENDMETHOD.

  METHOD read_languages.

    DATA lt_t002t     TYPE ty_t_t002t.
    DATA lt_installed TYPE string_table.

    z2ui5_cl_popup_msg_search=>read_t002( IMPORTING t002t     = lt_t002t
                                                    installed = lt_installed ).

    mt_langu = get_language_list( t002t     = lt_t002t
                                  installed = lt_installed ).

  ENDMETHOD.

  METHOD get_language_list.

    " the same list as the message search - see there
    result = z2ui5_cl_popup_msg_search=>language_list( t002t     = t002t
                                                       installed = installed
                                                       langu     = mv_langu
                                                       langu2    = mv_langu2 ).

  ENDMETHOD.

  METHOD search.

    DATA lt_dd04t TYPE ty_t_dd04t.
    DATA lv_search TYPE string.
    DATA lv_lower TYPE string.
    DATA lv_capital TYPE string.
    DATA lv_p1 TYPE string.
    DATA lv_p2 TYPE string.
    DATA lv_p3 TYPE string.
    DATA lv_p4 TYPE string.
    DATA temp54 TYPE sy-langu.
    DATA lv_langu LIKE temp54.
    DATA lv_tabname TYPE string.
    DATA lv_rows TYPE i.
        DATA lx TYPE REF TO cx_root.
    DATA temp55 TYPE z2ui5_cl_popup_dtel_srch=>ty_t_row.
    DATA dd04t LIKE LINE OF lt_dd04t.
      DATA temp56 LIKE LINE OF temp55.

    CLEAR mt_result.
    CLEAR mv_info.
    mv_check_info = abap_false.

    IF mv_check_disabled = abap_true.
      RETURN.
    ENDIF.


    lv_search = condense( mv_search ).
    IF lv_search IS INITIAL.
      client->message_toast_display( `Enter a search text.` ).
      RETURN.
    ENDIF.

    " DD04T has no upper-case text field, so the text is searched as typed,
    " in upper and in lower case, and with a capital first letter

    lv_lower   = to_lower( lv_search ).

    lv_capital = |{ to_upper( substring( val = lv_lower len = 1 ) ) }{ substring( val = lv_lower off = 1 ) }|.

    lv_p1      = get_like_pattern( lv_search ).

    lv_p2      = get_like_pattern( to_upper( lv_search ) ).

    lv_p3      = get_like_pattern( lv_lower ).

    lv_p4      = get_like_pattern( lv_capital ).

    temp54 = mv_langu.

    lv_langu = temp54.

    lv_tabname = `DD04T`.
    " one row more than shown, to tell whether the result was cut

    lv_rows    = max_rows + 1.

    TRY.
        SELECT rollname ddlanguage ddtext reptext scrtext_s scrtext_m scrtext_l
          FROM (lv_tabname) UP TO lv_rows ROWS INTO CORRESPONDING FIELDS OF TABLE lt_dd04t
          WHERE ddlanguage = lv_langu
            AND as4local   = 'A'
            AND (    rollname  LIKE lv_p2 ESCAPE '#'
                  OR ddtext    LIKE lv_p1 ESCAPE '#' OR ddtext    LIKE lv_p2 ESCAPE '#'
                  OR ddtext    LIKE lv_p3 ESCAPE '#' OR ddtext    LIKE lv_p4 ESCAPE '#'
                  OR reptext   LIKE lv_p1 ESCAPE '#' OR reptext   LIKE lv_p2 ESCAPE '#'
                  OR reptext   LIKE lv_p3 ESCAPE '#' OR reptext   LIKE lv_p4 ESCAPE '#'
                  OR scrtext_s LIKE lv_p1 ESCAPE '#' OR scrtext_s LIKE lv_p2 ESCAPE '#'
                  OR scrtext_s LIKE lv_p3 ESCAPE '#' OR scrtext_s LIKE lv_p4 ESCAPE '#'
                  OR scrtext_m LIKE lv_p1 ESCAPE '#' OR scrtext_m LIKE lv_p2 ESCAPE '#'
                  OR scrtext_m LIKE lv_p3 ESCAPE '#' OR scrtext_m LIKE lv_p4 ESCAPE '#'
                  OR scrtext_l LIKE lv_p1 ESCAPE '#' OR scrtext_l LIKE lv_p2 ESCAPE '#'
                  OR scrtext_l LIKE lv_p3 ESCAPE '#' OR scrtext_l LIKE lv_p4 ESCAPE '#' )
          ORDER BY rollname

          .

      CATCH cx_root INTO lx.
        client->message_toast_display( |Search failed: { lx->get_text( ) }| ).
        RETURN.
    ENDTRY.

    DELETE ADJACENT DUPLICATES FROM lt_dd04t COMPARING rollname.
    IF lines( lt_dd04t ) > max_rows.
      DELETE lt_dd04t INDEX lv_rows.
      mv_info = |More than { max_rows } data elements found - only the first { max_rows } are shown, refine the search.|.
      mv_check_info = abap_true.
    ENDIF.


    CLEAR temp55.

    LOOP AT lt_dd04t INTO dd04t.

      temp56-rollname = dd04t-rollname.
      temp56-description = dd04t-ddtext.
      temp56-short = dd04t-scrtext_s.
      temp56-medium = dd04t-scrtext_m.
      temp56-long = dd04t-scrtext_l.
      temp56-heading = dd04t-reptext.
      INSERT temp56 INTO TABLE temp55.
    ENDLOOP.
    mt_result = temp55.

    IF mt_result IS INITIAL.
      client->message_toast_display( `No data elements found.` ).
      RETURN.
    ENDIF.

    read_details( ).

    apply_min_languages( ).

  ENDMETHOD.

  METHOD read_details.

    TYPES:
      BEGIN OF ty_s_count,
        rollname   TYPE c LENGTH 30,
        ddlanguage TYPE c LENGTH 1,
      END OF ty_s_count.
    TYPES temp1 TYPE STANDARD TABLE OF ty_s_count WITH DEFAULT KEY.
DATA lt_count TYPE temp1.
    DATA lt_text2 TYPE ty_t_dd04t.
    DATA lt_dd04l TYPE ty_t_dd04l.
    TYPES temp2 TYPE RANGE OF ty_s_row-rollname.
DATA lt_r_rollname TYPE temp2.

    " a range of at most max_rows names instead of FOR ALL ENTRIES
    DATA temp57 LIKE lt_r_rollname.
    DATA row LIKE LINE OF mt_result.
      DATA temp58 LIKE LINE OF temp57.
    DATA temp59 TYPE sy-langu.
    DATA lv_langu2 LIKE temp59.
    DATA lv_dd04t TYPE string.
    DATA lv_dd04l TYPE string.
        DATA lx TYPE REF TO cx_root.
    DATA temp60 LIKE LINE OF mt_result.
    DATA lr_row LIKE REF TO temp60.
      DATA temp61 TYPE z2ui5_cl_popup_dtel_srch=>ty_s_dd04t.
      DATA temp62 TYPE z2ui5_cl_popup_dtel_srch=>ty_s_dd04t.
      DATA ls_text2 LIKE temp61.
      DATA temp63 TYPE z2ui5_cl_popup_dtel_srch=>ty_s_dd04l.
      DATA temp64 TYPE z2ui5_cl_popup_dtel_srch=>ty_s_dd04l.
      DATA ls_dd04l LIKE temp63.
        DATA temp65 TYPE i.
      DATA temp66 TYPE i.
      DATA n TYPE i.
      DATA count LIKE LINE OF lt_count.
    CLEAR temp57.

    LOOP AT mt_result INTO row.

      temp58-sign = `I`.
      temp58-option = `EQ`.
      temp58-low = row-rollname.
      INSERT temp58 INTO TABLE temp57.
    ENDLOOP.
    lt_r_rollname = temp57.
    IF lt_r_rollname IS INITIAL.
      RETURN.
    ENDIF.


    temp59 = mv_langu2.

    lv_langu2 = temp59.

    lv_dd04t  = `DD04T`.

    lv_dd04l  = `DD04L`.

    TRY.
        SELECT rollname ddlanguage ddtext reptext scrtext_s scrtext_m scrtext_l
          FROM (lv_dd04t) INTO CORRESPONDING FIELDS OF TABLE lt_text2
          WHERE rollname   IN lt_r_rollname
            AND ddlanguage = lv_langu2
            AND as4local   = 'A'
          ORDER BY rollname ddlanguage
          .

        SELECT rollname ddlanguage
          FROM (lv_dd04t) INTO CORRESPONDING FIELDS OF TABLE lt_count
          WHERE rollname IN lt_r_rollname
            AND as4local = 'A'
          ORDER BY rollname ddlanguage
          .

        SELECT rollname datatype leng
          FROM (lv_dd04l) INTO CORRESPONDING FIELDS OF TABLE lt_dd04l
          WHERE rollname IN lt_r_rollname
            AND as4local = 'A'
          ORDER BY rollname
          .

      CATCH cx_root INTO lx.
        client->message_toast_display( |Details could not be read: { lx->get_text( ) }| ).
    ENDTRY.



    LOOP AT mt_result REFERENCE INTO lr_row.


      CLEAR temp61.

      READ TABLE lt_text2 INTO temp62 WITH KEY rollname = lr_row->rollname.
      IF sy-subrc = 0.
        temp61 = temp62.
      ENDIF.

      ls_text2 = temp61.
      lr_row->description2 = ls_text2-ddtext.
      lr_row->long2        = ls_text2-scrtext_l.


      CLEAR temp63.

      READ TABLE lt_dd04l INTO temp64 WITH KEY rollname = lr_row->rollname.
      IF sy-subrc = 0.
        temp63 = temp64.
      ENDIF.

      ls_dd04l = temp63.
      lr_row->datatype = ls_dd04l-datatype.
      IF ls_dd04l-leng IS NOT INITIAL.

        temp65 = ls_dd04l-leng.
        lr_row->leng = |{ temp65 }|.
      ENDIF.



      n = 0.

      LOOP AT lt_count INTO count WHERE rollname = lr_row->rollname.
        n = n + 1.
      ENDLOOP.
      temp66 = n.
      lr_row->count = temp66.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_like_pattern.

    result = z2ui5_cl_popup_msg_search=>like_pattern( val      = val
                                                      contains = contains ).

  ENDMETHOD.

  METHOD display.

    TYPES:
      BEGIN OF ty_s_col,
        name  TYPE string,
        text  TYPE string,
        width TYPE string,
      END OF ty_s_col.
    TYPES temp3 TYPE STANDARD TABLE OF ty_s_col WITH DEFAULT KEY.
DATA lt_col TYPE temp3.
    DATA popup   TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA dialog  TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA content TYPE REF TO z2ui5_cl_ui5_view_builder.

    DATA embedded TYPE abap_bool.
    DATA temp1 TYPE xsdboolean.
    DATA temp2 TYPE xsdboolean.
    DATA temp5 TYPE xsdboolean.
    DATA temp6 TYPE xsdboolean.
    DATA temp67 TYPE string.
    DATA table TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA temp68 LIKE lt_col.
    DATA temp69 LIKE LINE OF temp68.
    DATA columns TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA cells TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA ls_col LIKE LINE OF lt_col.
    DATA temp4 TYPE xsdboolean.
    temp1 = boolc( z2ui5_if_popup_embed~mo_parent_view IS BOUND ).
    embedded = temp1.

    IF embedded = abap_true.

      " embedded: into the host's page; the rows are not selectable, there
      " is no OK to take a row
      content = z2ui5_if_popup_embed~mo_parent_view->ele( `VBox`
                                 )->a( n = `class` v = `sapUiSmallMargin` ).

    ELSE.

      popup = z2ui5_cl_ui5_view_builder=>factory(
                  )->ele( n = `FragmentDefinition` ns = `core`
                  )->a( n = `xmlns` v = `sap.m`
                  )->a( n = `xmlns:core` v = `sap.ui.core` ).

      dialog = popup->ele( `Dialog`
                       )->a( n = `title` t = title
                       )->a( n = `contentWidth` v = `90%`
                       )->a( n = `contentHeight` v = `80%`
                       )->a( n = `afterClose` v = client->_event( `CANCEL` ) ).

      content = dialog->ele( `content`
                         )->ele( `VBox`
                         )->a( n = `class` v = `sapUiSmallMargin` ).

    ENDIF.

    content->tag( `MessageStrip`
        )->a( n = `text` v = client->_bind( mv_info )
        )->a( n = `type` v = `Warning`
        )->a( n = `visible` v = client->_bind( mv_check_info ) ).


    temp2 = boolc( mv_check_disabled = abap_false ).

    temp5 = boolc( mv_check_disabled = abap_false ).

    temp6 = boolc( mv_check_disabled = abap_false ).
    content->ele( `HBox`
        )->a( n = `alignItems` v = `Center`
        )->a( n = `wrap` v = `Wrap`
        )->tag( `SearchField`
        )->a( n = `value` v = client->_bind( mv_search )
        )->a( n = `search` v = client->_event( `SEARCH` )
        )->a( n = `placeholder` v = `Text or name of the data element, * as wildcard`
        )->a( n = `enabled` b = temp2
        )->a( n = `width` v = `30rem`
        )->tag( `Label`
        )->a( n = `text` v = `Language`
        )->a( n = `class` v = `sapUiSmallMarginBegin sapUiTinyMarginEnd`
        )->ele( `Select`
        )->a( n = `selectedKey` v = client->_bind( mv_langu )
        )->a( n = `items` v = client->_bind( mt_langu )
        )->a( n = `change` v = client->_event( `SEARCH` )
        )->a( n = `enabled` b = temp5
        )->tag( n = `Item` ns = `core`
        )->a( n = `key` v = `{KEY}`
        )->a( n = `text` v = `{TEXT}`
        )->end(
        )->tag( `Label`
        )->a( n = `text` v = `2nd Language`
        )->a( n = `class` v = `sapUiSmallMarginBegin sapUiTinyMarginEnd`
        )->ele( `Select`
        )->a( n = `selectedKey` v = client->_bind( mv_langu2 )
        )->a( n = `items` v = client->_bind( mt_langu )
        )->a( n = `change` v = client->_event( `SEARCH` )
        )->a( n = `enabled` b = temp6
        )->tag( n = `Item` ns = `core`
        )->a( n = `key` v = `{KEY}`
        )->a( n = `text` v = `{TEXT}` ).


    IF embedded = abap_true.
      temp67 = `None`.
    ELSE.
      temp67 = `SingleSelectLeft`.
    ENDIF.

    table = content->ele( `Table`
                             )->a( n = `mode` v = temp67
                             )->a( n = `items` v = client->_bind( mt_result )
                             )->a( n = `growing` b = abap_true
                             )->a( n = `growingThreshold` v = `50`
                             )->a( n = `noDataText` v = `Enter a search text and press Enter` ).


    CLEAR temp68.

    CLEAR temp69.
    temp69-name = `ROLLNAME`.
    temp69-text = `Data Element`.
    temp69-width = `14rem`.
    INSERT temp69 INTO TABLE temp68.
    CLEAR temp69.
    temp69-name = `DATATYPE`.
    temp69-text = `Type`.
    temp69-width = `5rem`.
    INSERT temp69 INTO TABLE temp68.
    CLEAR temp69.
    temp69-name = `LENG`.
    temp69-text = `Length`.
    temp69-width = `5rem`.
    INSERT temp69 INTO TABLE temp68.
    CLEAR temp69.
    temp69-name = `COUNT`.
    temp69-text = `Languages`.
    temp69-width = `6rem`.
    INSERT temp69 INTO TABLE temp68.
    CLEAR temp69.
    temp69-name = `DESCRIPTION`.
    temp69-text = `Description`.
    INSERT temp69 INTO TABLE temp68.
    CLEAR temp69.
    temp69-name = `SHORT`.
    temp69-text = `Short`.
    INSERT temp69 INTO TABLE temp68.
    CLEAR temp69.
    temp69-name = `MEDIUM`.
    temp69-text = `Medium`.
    INSERT temp69 INTO TABLE temp68.
    CLEAR temp69.
    temp69-name = `LONG`.
    temp69-text = `Long`.
    INSERT temp69 INTO TABLE temp68.
    CLEAR temp69.
    temp69-name = `HEADING`.
    temp69-text = `Heading`.
    INSERT temp69 INTO TABLE temp68.
    CLEAR temp69.
    temp69-name = `DESCRIPTION2`.
    temp69-text = `Description (2nd Language)`.
    INSERT temp69 INTO TABLE temp68.
    CLEAR temp69.
    temp69-name = `LONG2`.
    temp69-text = `Long (2nd Language)`.
    INSERT temp69 INTO TABLE temp68.
    lt_col = temp68.


    columns = table->ele( `columns` ).

    cells = table->ele( `items`
                           )->ele( `ColumnListItem`
                           )->a( n = `selected` v = `{SELKZ}`
                           )->ele( `cells` ).


    LOOP AT lt_col INTO ls_col.
      columns->ele( `Column`
          )->a( n = `width` v = ls_col-width
          )->tag( `Text`
          )->a( n = `text` t = ls_col-text ).
      cells->tag( `Text`
          )->a( n = `text` v = |\{{ z2ui5_cl_ui5_view_builder=>component_path( ls_col-name ) }\}| ).
    ENDLOOP.

    IF embedded = abap_true.
      RETURN.
    ENDIF.


    temp4 = boolc( mv_check_disabled = abap_false ).
    dialog->ele( `buttons`
        )->tag( `Button`
        )->a( n = `text` v = `Cancel`
        )->a( n = `press` v = client->_event( `CANCEL` )
        )->tag( `Button`
        )->a( n = `text` v = `OK`
        )->a( n = `press` v = client->_event( `CONFIRM` )
        )->a( n = `enabled` b = temp4
        )->a( n = `type` v = `Emphasized` ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~check_confirmed.

    result = ms_result-check_confirmed.

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~value.

    GET REFERENCE OF ms_result INTO result.

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~value_string.

    result = ms_result-rollname.

  ENDMETHOD.

ENDCLASS.
