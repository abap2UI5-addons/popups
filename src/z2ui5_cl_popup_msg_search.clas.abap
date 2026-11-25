"! Developer search popup for messages: searches T100 by text and/or message
"! class and number in a chosen language and shows a second language side by
"! side. The caller reads the chosen message with result( ).
"!
"! T100 is reached by name only (dynamic FROM), so the class also compiles on
"! ABAP Cloud, where it is not released - there the popup shows that the
"! search is not available.
CLASS z2ui5_cl_popup_msg_search DEFINITION PUBLIC.

  PUBLIC SECTION.
    " abap2ui5lint-disable unbound-public-attribute -- a popup's attributes are its interface to the app that called it, which reads them once the popup returns
    INTERFACES z2ui5_if_app.
    " check_confirmed( ) - a message chosen; value( ) - REF TO ty_s_result,
    " message class, number and text (ms_result); value_string( ) - empty,
    " class and number are two values
    INTERFACES z2ui5_if_popup_result.

    TYPES:
      BEGIN OF ty_s_result,
        msgid           TYPE string,
        msgno           TYPE string,
        text            TYPE string,
        check_confirmed TYPE abap_bool,
      END OF ty_s_result.

    TYPES:
      BEGIN OF ty_s_row,
        selkz     TYPE abap_bool,
        arbgb     TYPE c LENGTH 20,
        msgnr     TYPE c LENGTH 3,
        count     TYPE i,
        text      TYPE string,
        text2     TYPE string,
        statement TYPE string,
      END OF ty_s_row.
    TYPES ty_t_row TYPE STANDARD TABLE OF ty_s_row WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_langu,
        key  TYPE string,
        text TYPE string,
      END OF ty_s_langu.
    TYPES ty_t_langu TYPE STANDARD TABLE OF ty_s_langu WITH EMPTY KEY.

    DATA ms_result         TYPE ty_s_result.
    DATA mv_search         TYPE string.
    DATA mv_msgid          TYPE string.
    DATA mv_msgno          TYPE string.
    DATA mv_langu          TYPE string.
    DATA mv_langu2         TYPE string.
    DATA mt_langu          TYPE ty_t_langu.
    DATA mt_result         TYPE ty_t_row.
    DATA mv_check_disabled TYPE abap_bool.
    DATA mv_info           TYPE string.
    DATA mv_check_info     TYPE abap_bool.

    "! @parameter i_search | prefilled message text; when a text, message
    "!                       class or number is supplied, the search runs as
    "!                       soon as the popup opens
    "! @parameter i_msgid | prefilled message class, * as wildcard
    "! @parameter i_msgno | prefilled message number, * as wildcard
    "! @parameter i_langu | language of the texts, default English
    "! @parameter i_langu2 | second language shown side by side, default
    "!                       German (English when i_langu is German)
    "! @parameter i_max_rows | maximum number of messages read
    CLASS-METHODS factory
      IMPORTING
        i_search          TYPE clike     OPTIONAL
        i_msgid           TYPE clike     OPTIONAL
        i_msgno           TYPE clike     OPTIONAL
        i_langu           TYPE sy-langu  DEFAULT 'E'
        i_langu2          TYPE sy-langu  OPTIONAL
        i_title           TYPE clike     DEFAULT `Message Search`
        i_max_rows        TYPE i         DEFAULT 200
        i_event_canceled  TYPE string    OPTIONAL
        i_event_confirmed TYPE string    OPTIONAL
          PREFERRED PARAMETER i_search
      RETURNING
        VALUE(r_result)   TYPE REF TO z2ui5_cl_popup_msg_search.

    METHODS result
      RETURNING
        VALUE(result) TYPE ty_s_result.
    " abap2ui5lint-enable unbound-public-attribute

  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_s_t100,
        sprsl TYPE c LENGTH 1,
        arbgb TYPE c LENGTH 20,
        msgnr TYPE c LENGTH 3,
        text  TYPE c LENGTH 73,
      END OF ty_s_t100.
    TYPES ty_t_t100 TYPE STANDARD TABLE OF ty_s_t100 WITH EMPTY KEY.

    DATA client          TYPE REF TO z2ui5_if_client.
    DATA title           TYPE string.
    DATA max_rows        TYPE i.
    DATA event_confirmed TYPE string.
    DATA event_canceled  TYPE string.

    METHODS on_init.
    METHODS on_event.
    METHODS on_event_confirm.
    METHODS display.
    METHODS search.
    METHODS read_languages.
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


CLASS z2ui5_cl_popup_msg_search IMPLEMENTATION.

  METHOD factory.

    r_result = NEW #( ).
    r_result->title           = i_title.
    r_result->mv_search       = i_search.
    r_result->mv_msgid        = i_msgid.
    r_result->mv_msgno        = i_msgno.
    r_result->mv_langu        = i_langu.
    r_result->mv_langu2       = COND #( WHEN i_langu2 IS NOT INITIAL THEN i_langu2
                                        WHEN i_langu = `D` THEN `E`
                                        ELSE `D` ).
    r_result->max_rows        = COND #( WHEN i_max_rows > 0 THEN i_max_rows ELSE 200 ).
    r_result->event_canceled  = i_event_canceled.
    r_result->event_confirmed = i_event_confirmed.

  ENDMETHOD.

  METHOD result.

    result = ms_result.

  ENDMETHOD.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ).
      on_init( ).
      display( ).
      RETURN.
    ENDIF.

    on_event( ).

  ENDMETHOD.

  METHOD on_init.

    IF z2ui5_cl_popup_context=>check_abap_cloud( ).
      mv_check_disabled = abap_true.
      mv_info = `The message search is not available on ABAP Cloud: T100 is not released there.`.
      mv_check_info = abap_true.
      RETURN.
    ENDIF.

    " started without factory( ) (CREATE OBJECT from customizing): the
    " defaults of factory( )
    IF mv_langu IS INITIAL.
      mv_langu = `E`.
    ENDIF.
    IF mv_langu2 IS INITIAL.
      mv_langu2 = COND #( WHEN mv_langu = `D` THEN `E` ELSE `D` ).
    ENDIF.
    IF max_rows <= 0.
      max_rows = 200.
    ENDIF.
    IF title IS INITIAL.
      title = `Message Search`.
    ENDIF.

    read_languages( ).

    IF mv_search IS NOT INITIAL OR mv_msgid IS NOT INITIAL OR mv_msgno IS NOT INITIAL.
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

    READ TABLE mt_result INTO DATA(ls_row) WITH KEY selkz = abap_true.
    IF sy-subrc <> 0.
      client->message_toast_display( `Select a message first.` ).
      RETURN.
    ENDIF.

    ms_result = VALUE #( msgid           = ls_row-arbgb
                         msgno           = ls_row-msgnr
                         text            = ls_row-text
                         check_confirmed = abap_true ).

    client->popup_destroy( ).
    client->nav_app_leave( event = event_confirmed ).

  ENDMETHOD.

  METHOD read_languages.

    TYPES:
      BEGIN OF ty_s_t002t,
        sprsl TYPE c LENGTH 1,
        sptxt TYPE c LENGTH 16,
      END OF ty_s_t002t.
    DATA lt_t002t TYPE STANDARD TABLE OF ty_s_t002t WITH EMPTY KEY.

    TRY.
        DATA(lv_tabname) = `T002T`.
        SELECT sprsl, sptxt
          FROM (lv_tabname)
          WHERE spras = @sy-langu
          ORDER BY sprsl
          INTO CORRESPONDING FIELDS OF TABLE @lt_t002t.
      CATCH cx_root ##NO_HANDLER.
        " T002T not readable - the fallback list below is used
    ENDTRY.

    " only the languages installed on the system (T002C-LAINST); if T002C
    " cannot be read, every language of T002T stays
    TYPES:
      BEGIN OF ty_s_t002c,
        spras TYPE c LENGTH 1,
      END OF ty_s_t002c.
    DATA lt_t002c TYPE STANDARD TABLE OF ty_s_t002c WITH EMPTY KEY.
    TRY.
        DATA(lv_tabname_c) = `T002C`.
        SELECT spras
          FROM (lv_tabname_c)
          WHERE lainst = @abap_true
          ORDER BY spras
          INTO CORRESPONDING FIELDS OF TABLE @lt_t002c.
      CATCH cx_root ##NO_HANDLER.
        " T002C not readable - no filter
    ENDTRY.
    IF lt_t002c IS NOT INITIAL.
      LOOP AT lt_t002t INTO DATA(ls_t002t).
        IF NOT line_exists( lt_t002c[ spras = ls_t002t-sprsl ] ).
          DELETE TABLE lt_t002t FROM ls_t002t.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SORT lt_t002t BY sptxt.
    mt_langu = VALUE #( FOR t002t IN lt_t002t
                        ( key = t002t-sprsl text = |{ t002t-sptxt } ({ t002t-sprsl })| ) ).

    IF mt_langu IS INITIAL.
      mt_langu = VALUE #( ( key = `E` text = `English (E)` ) ).
      IF sy-langu <> `E`.
        INSERT VALUE #( key = sy-langu text = |Logon Language ({ sy-langu })| ) INTO mt_langu INDEX 1.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD search.

    DATA lt_t100 TYPE ty_t_t100.

    CLEAR mt_result.
    CLEAR mv_info.
    mv_check_info = abap_false.

    IF mv_check_disabled = abap_true.
      RETURN.
    ENDIF.

    DATA(lv_search) = condense( mv_search ).
    DATA(lv_msgid)  = to_upper( condense( mv_msgid ) ).
    DATA(lv_msgno)  = condense( mv_msgno ).
    IF lv_search IS INITIAL AND lv_msgid IS INITIAL AND lv_msgno IS INITIAL.
      client->message_toast_display( `Enter a text, a message class or a message number.` ).
      RETURN.
    ENDIF.

    " T100 has no upper-case text field, so the text is searched as typed,
    " in upper and in lower case, and with a capital first letter
    IF lv_search IS INITIAL.
      DATA(lv_p1) = `%`.
      DATA(lv_p2) = `%`.
      DATA(lv_p3) = `%`.
      DATA(lv_p4) = `%`.
    ELSE.
      DATA(lv_lower)   = to_lower( lv_search ).
      DATA(lv_capital) = |{ to_upper( substring( val = lv_lower len = 1 ) ) }{ substring( val = lv_lower off = 1 ) }|.
      lv_p1 = get_like_pattern( lv_search ).
      lv_p2 = get_like_pattern( to_upper( lv_search ) ).
      lv_p3 = get_like_pattern( lv_lower ).
      lv_p4 = get_like_pattern( lv_capital ).
    ENDIF.

    " message class and number match exactly, unless they carry a *
    DATA(lv_msgid_pattern) = COND string( WHEN lv_msgid IS INITIAL THEN `%`
                                          ELSE get_like_pattern( val = lv_msgid contains = abap_false ) ).
    DATA(lv_msgno_pattern) = COND string( WHEN lv_msgno IS INITIAL THEN `%`
                                          ELSE get_like_pattern( val = lv_msgno contains = abap_false ) ).

    DATA(lv_langu)   = CONV sy-langu( mv_langu ).
    DATA(lv_tabname) = `T100`.
    " one row more than shown, to tell whether the result was cut
    DATA(lv_rows)    = max_rows + 1.

    TRY.
        SELECT sprsl, arbgb, msgnr, text
          FROM (lv_tabname)
          WHERE sprsl = @lv_langu
            AND arbgb LIKE @lv_msgid_pattern ESCAPE '#'
            AND msgnr LIKE @lv_msgno_pattern ESCAPE '#'
            AND (    text LIKE @lv_p1 ESCAPE '#'
                  OR text LIKE @lv_p2 ESCAPE '#'
                  OR text LIKE @lv_p3 ESCAPE '#'
                  OR text LIKE @lv_p4 ESCAPE '#' )
          ORDER BY arbgb, msgnr
          INTO CORRESPONDING FIELDS OF TABLE @lt_t100
          UP TO @lv_rows ROWS.
      CATCH cx_root INTO DATA(lx).
        client->message_toast_display( |Search failed: { lx->get_text( ) }| ).
        RETURN.
    ENDTRY.

    IF lines( lt_t100 ) > max_rows.
      DELETE lt_t100 INDEX lv_rows.
      mv_info = |More than { max_rows } messages found - only the first { max_rows } are shown, refine the search.|.
      mv_check_info = abap_true.
    ENDIF.

    mt_result = VALUE #( FOR t100 IN lt_t100
                         ( arbgb = t100-arbgb
                           msgnr = t100-msgnr
                           text  = t100-text ) ).

    IF mt_result IS INITIAL.
      client->message_toast_display( `No messages found.` ).
      RETURN.
    ENDIF.

    read_details( ).

  ENDMETHOD.

  METHOD read_details.

    TYPES:
      BEGIN OF ty_s_count,
        arbgb TYPE c LENGTH 20,
        msgnr TYPE c LENGTH 3,
        cnt   TYPE i,
      END OF ty_s_count.
    DATA lt_count   TYPE STANDARD TABLE OF ty_s_count WITH EMPTY KEY.
    DATA lt_text2   TYPE ty_t_t100.
    DATA lt_r_arbgb TYPE RANGE OF ty_s_count-arbgb.
    DATA lt_r_msgnr TYPE RANGE OF ty_s_count-msgnr.

    " ranges of the found classes and numbers instead of FOR ALL ENTRIES;
    " they select a superset, the exact pairs are matched below
    lt_r_arbgb = VALUE #( FOR row IN mt_result
                          ( sign = `I` option = `EQ` low = row-arbgb ) ).
    SORT lt_r_arbgb BY low.
    DELETE ADJACENT DUPLICATES FROM lt_r_arbgb COMPARING low.
    lt_r_msgnr = VALUE #( FOR row IN mt_result
                          ( sign = `I` option = `EQ` low = row-msgnr ) ).
    SORT lt_r_msgnr BY low.
    DELETE ADJACENT DUPLICATES FROM lt_r_msgnr COMPARING low.
    IF lt_r_arbgb IS INITIAL OR lt_r_msgnr IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_langu2)  = CONV sy-langu( mv_langu2 ).
    DATA(lv_tabname) = `T100`.

    TRY.
        SELECT sprsl, arbgb, msgnr, text
          FROM (lv_tabname)
          WHERE sprsl = @lv_langu2
            AND arbgb IN @lt_r_arbgb
            AND msgnr IN @lt_r_msgnr
          ORDER BY arbgb, msgnr
          INTO CORRESPONDING FIELDS OF TABLE @lt_text2.

        SELECT arbgb, msgnr, COUNT( * ) AS cnt
          FROM (lv_tabname)
          WHERE arbgb IN @lt_r_arbgb
            AND msgnr IN @lt_r_msgnr
          GROUP BY arbgb, msgnr
          ORDER BY arbgb, msgnr
          INTO CORRESPONDING FIELDS OF TABLE @lt_count.
      CATCH cx_root INTO DATA(lx).
        client->message_toast_display( |Details could not be read: { lx->get_text( ) }| ).
    ENDTRY.

    LOOP AT mt_result REFERENCE INTO DATA(lr_row).

      lr_row->text2 = VALUE #( lt_text2[ arbgb = lr_row->arbgb
                                         msgnr = lr_row->msgnr ]-text OPTIONAL ).
      lr_row->count = VALUE #( lt_count[ arbgb = lr_row->arbgb
                                         msgnr = lr_row->msgnr ]-cnt OPTIONAL ).

      " the statement that raises the message, with a WITH for each placeholder
      DATA(lv_count) = count( val = lr_row->text sub = `&` ).
      DATA(lv_with) = ``.
      DO lv_count TIMES.
        lv_with = |{ lv_with } v{ sy-index }|.
        IF sy-index = 4.
          EXIT.
        ENDIF.
      ENDDO.
      lr_row->statement = |MESSAGE e{ lr_row->msgnr }({ to_lower( condense( lr_row->arbgb ) ) })|.
      IF lv_with IS NOT INITIAL.
        lr_row->statement = |{ lr_row->statement } WITH{ lv_with }|.
      ENDIF.
      lr_row->statement = |{ lr_row->statement }.|.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_like_pattern.

    result = val.
    REPLACE ALL OCCURRENCES OF `#` IN result WITH `##`.
    REPLACE ALL OCCURRENCES OF `%` IN result WITH `#%`.
    REPLACE ALL OCCURRENCES OF `_` IN result WITH `#_`.
    REPLACE ALL OCCURRENCES OF `*` IN result WITH `%`.

    IF contains = abap_true.
      result = |%{ result }%|.
    ENDIF.

  ENDMETHOD.

  METHOD display.

    TYPES:
      BEGIN OF ty_s_col,
        name  TYPE string,
        text  TYPE string,
        width TYPE string,
      END OF ty_s_col.
    DATA lt_col TYPE STANDARD TABLE OF ty_s_col WITH EMPTY KEY.

    DATA(popup) = z2ui5_cl_ui5_view_builder=>factory(
                      )->ele( n = `FragmentDefinition` ns = `core`
                      )->a( n = `xmlns` v = `sap.m`
                      )->a( n = `xmlns:core` v = `sap.ui.core` ).

    DATA(dialog) = popup->ele( `Dialog`
                            )->a( n = `title` t = title
                            )->a( n = `contentWidth` v = `90%`
                            )->a( n = `contentHeight` v = `80%`
                            )->a( n = `afterClose` v = client->_event( `CANCEL` ) ).

    DATA(content) = dialog->ele( `content`
                              )->ele( `VBox`
                              )->a( n = `class` v = `sapUiSmallMargin` ).

    content->tag( `MessageStrip`
        )->a( n = `text` v = client->_bind( mv_info )
        )->a( n = `type` v = `Warning`
        )->a( n = `visible` v = client->_bind( mv_check_info ) ).

    content->ele( `HBox`
        )->a( n = `alignItems` v = `Center`
        )->a( n = `wrap` v = `Wrap`
        )->tag( `SearchField`
        )->a( n = `value` v = client->_bind( mv_search )
        )->a( n = `search` v = client->_event( `SEARCH` )
        )->a( n = `placeholder` v = `Message text, * as wildcard`
        )->a( n = `enabled` b = xsdbool( mv_check_disabled = abap_false )
        )->a( n = `width` v = `25rem`
        )->tag( `Label`
        )->a( n = `text` v = `Message Class`
        )->a( n = `class` v = `sapUiSmallMarginBegin sapUiTinyMarginEnd`
        )->tag( `Input`
        )->a( n = `value` v = client->_bind( mv_msgid )
        )->a( n = `submit` v = client->_event( `SEARCH` )
        )->a( n = `enabled` b = xsdbool( mv_check_disabled = abap_false )
        )->a( n = `width` v = `12rem`
        )->tag( `Label`
        )->a( n = `text` v = `Number`
        )->a( n = `class` v = `sapUiSmallMarginBegin sapUiTinyMarginEnd`
        )->tag( `Input`
        )->a( n = `value` v = client->_bind( mv_msgno )
        )->a( n = `submit` v = client->_event( `SEARCH` )
        )->a( n = `enabled` b = xsdbool( mv_check_disabled = abap_false )
        )->a( n = `width` v = `5rem`
        )->tag( `Label`
        )->a( n = `text` v = `Language`
        )->a( n = `class` v = `sapUiSmallMarginBegin sapUiTinyMarginEnd`
        )->ele( `Select`
        )->a( n = `selectedKey` v = client->_bind( mv_langu )
        )->a( n = `items` v = client->_bind( mt_langu )
        )->a( n = `change` v = client->_event( `SEARCH` )
        )->a( n = `enabled` b = xsdbool( mv_check_disabled = abap_false )
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
        )->a( n = `enabled` b = xsdbool( mv_check_disabled = abap_false )
        )->tag( n = `Item` ns = `core`
        )->a( n = `key` v = `{KEY}`
        )->a( n = `text` v = `{TEXT}`
        )->end(
        )->tag( `Button`
        )->a( n = `text` v = `Search`
        )->a( n = `icon` v = `sap-icon://search`
        )->a( n = `press` v = client->_event( `SEARCH` )
        )->a( n = `enabled` b = xsdbool( mv_check_disabled = abap_false )
        )->a( n = `class` v = `sapUiSmallMarginBegin` ).

    DATA(table) = content->ele( `Table`
                             )->a( n = `mode` v = `SingleSelectLeft`
                             )->a( n = `items` v = client->_bind( mt_result )
                             )->a( n = `growing` b = abap_true
                             )->a( n = `growingThreshold` v = `50`
                             )->a( n = `noDataText` v = `Enter a text, a message class or a number and press Enter` ).

    lt_col = VALUE #( ( name = `ARBGB`     text = `Message Class`        width = `12rem` )
                      ( name = `MSGNR`     text = `Number`               width = `5rem` )
                      ( name = `COUNT`     text = `Languages`            width = `6rem` )
                      ( name = `TEXT`      text = `Text` )
                      ( name = `TEXT2`     text = `Text (2nd Language)` )
                      ( name = `STATEMENT` text = `Statement` ) ).

    DATA(columns) = table->ele( `columns` ).
    DATA(cells) = table->ele( `items`
                           )->ele( `ColumnListItem`
                           )->a( n = `selected` v = `{SELKZ}`
                           )->ele( `cells` ).

    LOOP AT lt_col INTO DATA(ls_col).
      columns->ele( `Column`
          )->a( n = `width` v = ls_col-width
          )->tag( `Text`
          )->a( n = `text` t = ls_col-text ).
      cells->tag( `Text`
          )->a( n = `text` v = |\{{ ls_col-name }\}| ).
    ENDLOOP.

    dialog->ele( `buttons`
        )->tag( `Button`
        )->a( n = `text` v = `Cancel`
        )->a( n = `press` v = client->_event( `CANCEL` )
        )->tag( `Button`
        )->a( n = `text` v = `OK`
        )->a( n = `press` v = client->_event( `CONFIRM` )
        )->a( n = `enabled` b = xsdbool( mv_check_disabled = abap_false )
        )->a( n = `type` v = `Emphasized` ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~check_confirmed.

    result = ms_result-check_confirmed.

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~value.

    result = REF #( ms_result ).

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~value_string.

    " no single value - empty
    CLEAR result.

  ENDMETHOD.

ENDCLASS.
