"! Developer search popup for messages: searches T100 by text and/or message
"! class and number in a chosen language and shows a second language side by
"! side. The caller reads the chosen message with result( ).
"! Embedded (z2ui5_if_popup_embed) it is a search page inside a host app.
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
    " embedded mode: with mo_parent_view set by the host, the search renders
    " into the host's page - no popup, no OK/Cancel, no nav_app_leave( )
    INTERFACES z2ui5_if_popup_embed.

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
    TYPES ty_t_row TYPE STANDARD TABLE OF ty_s_row WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_langu,
        key  TYPE string,
        text TYPE string,
      END OF ty_s_langu.
    TYPES ty_t_langu TYPE STANDARD TABLE OF ty_s_langu WITH DEFAULT KEY.

    DATA ms_result         TYPE ty_s_result.
    DATA mv_search         TYPE string.
    DATA mv_msgid          TYPE string.
    DATA mv_msgno          TYPE string.
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

    "! @parameter i_search | prefilled message text; when a text, message
    "!                       class or number is supplied, the search runs as
    "!                       soon as the popup opens
    "! @parameter i_msgid | prefilled message class, * as wildcard
    "! @parameter i_msgno | prefilled message number, * as wildcard
    "! @parameter i_langu | language of the texts, default English
    "! @parameter i_langu2 | second language shown side by side, default
    "!                       German (English when i_langu is German)
    "! @parameter i_max_rows | maximum number of messages read
    "! @parameter i_min_languages | only hits with texts in at least this many
    "!                              languages (count of the Languages column); 0 = all
    CLASS-METHODS factory
      IMPORTING
        i_search          TYPE clike     OPTIONAL
        i_msgid           TYPE clike     OPTIONAL
        i_msgno           TYPE clike     OPTIONAL
        i_langu           TYPE sy-langu  DEFAULT 'E'
        i_langu2          TYPE sy-langu  OPTIONAL
        i_title           TYPE clike     DEFAULT `Message Search`
        i_max_rows        TYPE i         DEFAULT 200
        i_min_languages   TYPE i         DEFAULT 0
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
      BEGIN OF ty_s_t002t,
        sprsl TYPE c LENGTH 1,
        sptxt TYPE c LENGTH 16,
      END OF ty_s_t002t.
    TYPES ty_t_t002t TYPE STANDARD TABLE OF ty_s_t002t WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_t100,
        sprsl TYPE c LENGTH 1,
        arbgb TYPE c LENGTH 20,
        msgnr TYPE c LENGTH 3,
        text  TYPE c LENGTH 73,
      END OF ty_s_t100.
    TYPES ty_t_t100 TYPE STANDARD TABLE OF ty_s_t100 WITH DEFAULT KEY.

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


CLASS z2ui5_cl_popup_msg_search IMPLEMENTATION.

  METHOD factory.
    DATA temp48 TYPE string.
    DATA temp49 TYPE i.

    CREATE OBJECT r_result.
    r_result->title           = i_title.
    r_result->mv_search       = i_search.
    r_result->mv_msgid        = i_msgid.
    r_result->mv_msgno        = i_msgno.
    r_result->mv_langu        = i_langu.

    IF i_langu2 IS NOT INITIAL.
      temp48 = i_langu2.
    ELSEIF i_langu = `D`.
      temp48 = `E`.
    ELSE.
      temp48 = `D`.
    ENDIF.
    r_result->mv_langu2       = temp48.

    IF i_max_rows > 0.
      temp49 = i_max_rows.
    ELSE.
      temp49 = 200.
    ENDIF.
    r_result->max_rows        = temp49.
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
      DATA temp50 TYPE string.

    IF z2ui5_cl_popup_context=>check_abap_cloud( ) IS NOT INITIAL.
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

      IF mv_langu = `D`.
        temp50 = `E`.
      ELSE.
        temp50 = `D`.
      ENDIF.
      mv_langu2 = temp50.
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

    FIELD-SYMBOLS <ls_row> TYPE z2ui5_cl_popup_msg_search=>ty_s_row.
    READ TABLE mt_result WITH KEY selkz = abap_true ASSIGNING <ls_row>.
    IF <ls_row> IS NOT ASSIGNED.
      client->message_toast_display( `Select a message first.` ).
      RETURN.
    ENDIF.

    CLEAR ms_result.
    ms_result-msgid = <ls_row>-arbgb.
    ms_result-msgno = <ls_row>-msgnr.
    ms_result-text = <ls_row>-text.
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
      client->message_toast_display( |No messages with texts in at least { mv_min_languages } languages found.| ).
    ENDIF.

  ENDMETHOD.

  METHOD read_languages.

    DATA lt_t002t TYPE ty_t_t002t.
    DATA lt_installed TYPE string_table.
        DATA lv_tabname TYPE string.
TYPES BEGIN OF ty_s_t002c.
TYPES spras TYPE c LENGTH 1.
TYPES END OF ty_s_t002c.
    TYPES temp1 TYPE STANDARD TABLE OF ty_s_t002c WITH DEFAULT KEY.
DATA lt_t002c TYPE temp1.
        DATA lv_tabname_c TYPE string.
    DATA temp51 TYPE string_table.
    DATA t002c LIKE LINE OF lt_t002c.
      DATA temp12 LIKE LINE OF temp51.

    TRY.

        lv_tabname = `T002T`.
        SELECT sprsl sptxt
          FROM (lv_tabname) INTO CORRESPONDING FIELDS OF TABLE lt_t002t
          WHERE spras = sy-langu
          ORDER BY sprsl
          .
      CATCH cx_root ##NO_HANDLER.
        " T002T not readable - get_language_list( ) falls back to its texts
    ENDTRY.

    " the languages installed on the system (T002C-LAINST); if T002C cannot
    " be read, every language of T002T stays



    TRY.

        lv_tabname_c = `T002C`.
        SELECT spras
          FROM (lv_tabname_c) INTO CORRESPONDING FIELDS OF TABLE lt_t002c
          WHERE lainst = abap_true
          ORDER BY spras
          .
      CATCH cx_root ##NO_HANDLER.
        " T002C not readable - no filter
    ENDTRY.

    CLEAR temp51.

    LOOP AT lt_t002c INTO t002c.

      temp12 = t002c-spras.
      INSERT temp12 INTO TABLE temp51.
    ENDLOOP.
    lt_installed = temp51.

    mt_langu = get_language_list( t002t     = lt_t002t
                                  installed = lt_installed ).

  ENDMETHOD.

  METHOD get_language_list.

    " English, German, the logon language and the languages selected now are
    " always offered, also when T002C does not flag them - the defaults are E
    " and D, and a Select needs an entry for its selectedKey
    DATA temp53 TYPE string_table.
    DATA temp13 LIKE LINE OF temp53.
    DATA lt_always LIKE temp53.
    DATA lt_rest TYPE ty_t_langu.
    TYPES temp2 TYPE RANGE OF ty_s_t002t-sprsl.
DATA lr_installed TYPE temp2.
    DATA lt_t002t LIKE t002t.
      DATA temp55 LIKE lr_installed.
      DATA inst LIKE LINE OF installed.
        DATA temp56 LIKE LINE OF temp55.
      DATA temp57 LIKE lr_installed.
      DATA always LIKE LINE OF lt_always.
        DATA temp58 LIKE LINE OF temp57.
    DATA ls_t002t LIKE LINE OF lt_t002t.
      DATA temp59 TYPE string.
      DATA lv_key LIKE temp59.
      DATA temp60 LIKE sy-subrc.
      DATA temp61 TYPE z2ui5_cl_popup_msg_search=>ty_s_langu.
      DATA temp62 LIKE sy-subrc.
      DATA temp63 TYPE z2ui5_cl_popup_msg_search=>ty_s_langu.
    DATA temp14 TYPE string_table.
    DATA temp64 LIKE temp14.
      DATA temp65 TYPE z2ui5_cl_popup_msg_search=>ty_s_t002t-sptxt.
      DATA temp66 TYPE z2ui5_cl_popup_msg_search=>ty_s_t002t.
      DATA lv_text LIKE temp65.
        DATA temp67 TYPE z2ui5_cl_popup_msg_search=>ty_s_t002t-sptxt.
      DATA temp68 TYPE z2ui5_cl_popup_msg_search=>ty_s_langu.
    CLEAR temp53.
    INSERT `E` INTO TABLE temp53.
    INSERT `D` INTO TABLE temp53.

    temp13 = sy-langu.
    INSERT temp13 INTO TABLE temp53.
    INSERT mv_langu INTO TABLE temp53.
    INSERT mv_langu2 INTO TABLE temp53.

    lt_always = temp53.




    " only the installed languages - by a range: a DELETE TABLE ... FROM on
    " this table WITH EMPTY KEY removed the first line each time, not the
    " one of the loop

    lt_t002t = t002t.
    IF installed IS NOT INITIAL.

      CLEAR temp55.

      LOOP AT installed INTO inst.

        temp56-sign = `I`.
        temp56-option = `EQ`.
        temp56-low = inst.
        INSERT temp56 INTO TABLE temp55.
      ENDLOOP.
      lr_installed = temp55.

      CLEAR temp57.
      temp57 = lr_installed.

      LOOP AT lt_always INTO always WHERE table_line IS NOT INITIAL.

        temp58-sign = `I`.
        temp58-option = `EQ`.
        temp58-low = always.
        INSERT temp58 INTO TABLE temp57.
      ENDLOOP.
      lr_installed = temp57.
      DELETE lt_t002t WHERE sprsl NOT IN lr_installed.
    ENDIF.


    LOOP AT lt_t002t INTO ls_t002t.

      temp59 = ls_t002t-sprsl.

      lv_key = temp59.
      IF lv_key IS INITIAL OR lv_key = `E` OR lv_key = `D`.
        CONTINUE.
      ENDIF.

      READ TABLE lt_rest WITH KEY key = lv_key TRANSPORTING NO FIELDS.
      temp60 = sy-subrc.
      IF temp60 = 0.
        CONTINUE.
      ENDIF.

      CLEAR temp61.
      temp61-key = lv_key.
      temp61-text = |{ ls_t002t-sptxt } ({ lv_key })|.
      INSERT temp61 INTO TABLE lt_rest.
    ENDLOOP.

    " a language that must be offered but has no text in T002T
    LOOP AT lt_always INTO lv_key.

      READ TABLE lt_rest WITH KEY key = lv_key TRANSPORTING NO FIELDS.
      temp62 = sy-subrc.
      IF lv_key IS INITIAL OR lv_key = `E` OR lv_key = `D` OR temp62 = 0.
        CONTINUE.
      ENDIF.

      CLEAR temp63.
      temp63-key = lv_key.
      temp63-text = |Language ({ lv_key })|.
      INSERT temp63 INTO TABLE lt_rest.
    ENDLOOP.

    SORT lt_rest BY text.

    " English and German first, then the rest alphabetically

    CLEAR temp14.
    INSERT `E` INTO TABLE temp14.
    INSERT `D` INTO TABLE temp14.

    temp64 = temp14.
    LOOP AT temp64 INTO lv_key.

      CLEAR temp65.

      READ TABLE t002t INTO temp66 WITH KEY sprsl = lv_key.
      IF sy-subrc = 0.
        temp65 = temp66-sptxt.
      ENDIF.

      lv_text = temp65.
      IF lv_text IS INITIAL.

        IF lv_key = `E`.
          temp67 = `English`.
        ELSE.
          temp67 = `German`.
        ENDIF.
        lv_text = temp67.
      ENDIF.

      CLEAR temp68.
      temp68-key = lv_key.
      temp68-text = |{ lv_text } ({ lv_key })|.
      INSERT temp68 INTO TABLE result.
    ENDLOOP.
    INSERT LINES OF lt_rest INTO TABLE result.

  ENDMETHOD.

  METHOD search.

    DATA lt_t100 TYPE ty_t_t100.
    DATA lv_search TYPE string.
    DATA lv_msgid TYPE string.
    DATA lv_msgno TYPE string.
      DATA lv_p1 TYPE string.
      DATA lv_p2 TYPE string.
      DATA lv_p3 TYPE string.
      DATA lv_p4 TYPE string.
      DATA lv_lower TYPE string.
      DATA lv_capital TYPE string.
    DATA temp69 TYPE string.
    DATA lv_msgid_pattern LIKE temp69.
    DATA temp70 TYPE string.
    DATA lv_msgno_pattern LIKE temp70.
    DATA temp71 TYPE sy-langu.
    DATA lv_langu LIKE temp71.
    DATA lv_tabname TYPE string.
    DATA lv_rows TYPE i.
        DATA lx TYPE REF TO cx_root.
    DATA temp72 TYPE z2ui5_cl_popup_msg_search=>ty_t_row.
    DATA t100 LIKE LINE OF lt_t100.
      DATA temp73 LIKE LINE OF temp72.

    CLEAR mt_result.
    CLEAR mv_info.
    mv_check_info = abap_false.

    IF mv_check_disabled = abap_true.
      RETURN.
    ENDIF.


    lv_search = condense( mv_search ).

    lv_msgid  = to_upper( condense( mv_msgid ) ).

    lv_msgno  = condense( mv_msgno ).
    IF lv_search IS INITIAL AND lv_msgid IS INITIAL AND lv_msgno IS INITIAL.
      client->message_toast_display( `Enter a text, a message class or a message number.` ).
      RETURN.
    ENDIF.

    " T100 has no upper-case text field, so the text is searched as typed,
    " in upper and in lower case, and with a capital first letter
    IF lv_search IS INITIAL.

      lv_p1 = `%`.

      lv_p2 = `%`.

      lv_p3 = `%`.

      lv_p4 = `%`.
    ELSE.

      lv_lower   = to_lower( lv_search ).

      lv_capital = |{ to_upper( substring( val = lv_lower len = 1 ) ) }{ substring( val = lv_lower off = 1 ) }|.
      lv_p1 = get_like_pattern( lv_search ).
      lv_p2 = get_like_pattern( to_upper( lv_search ) ).
      lv_p3 = get_like_pattern( lv_lower ).
      lv_p4 = get_like_pattern( lv_capital ).
    ENDIF.

    " message class and number match exactly, unless they carry a *

    IF lv_msgid IS INITIAL.
      temp69 = `%`.
    ELSE.
      temp69 = get_like_pattern( val = lv_msgid contains = abap_false ).
    ENDIF.

    lv_msgid_pattern = temp69.

    IF lv_msgno IS INITIAL.
      temp70 = `%`.
    ELSE.
      temp70 = get_like_pattern( val = lv_msgno contains = abap_false ).
    ENDIF.

    lv_msgno_pattern = temp70.


    temp71 = mv_langu.

    lv_langu = temp71.

    lv_tabname = `T100`.
    " one row more than shown, to tell whether the result was cut

    lv_rows    = max_rows + 1.

    TRY.
        SELECT sprsl arbgb msgnr text
          FROM (lv_tabname) UP TO lv_rows ROWS INTO CORRESPONDING FIELDS OF TABLE lt_t100
          WHERE sprsl = lv_langu
            AND arbgb LIKE lv_msgid_pattern ESCAPE '#'
            AND msgnr LIKE lv_msgno_pattern ESCAPE '#'
            AND (    text LIKE lv_p1 ESCAPE '#'
                  OR text LIKE lv_p2 ESCAPE '#'
                  OR text LIKE lv_p3 ESCAPE '#'
                  OR text LIKE lv_p4 ESCAPE '#' )
          ORDER BY arbgb msgnr

          .

      CATCH cx_root INTO lx.
        client->message_toast_display( |Search failed: { lx->get_text( ) }| ).
        RETURN.
    ENDTRY.

    IF lines( lt_t100 ) > max_rows.
      DELETE lt_t100 INDEX lv_rows.
      mv_info = |More than { max_rows } messages found - only the first { max_rows } are shown, refine the search.|.
      mv_check_info = abap_true.
    ENDIF.


    CLEAR temp72.

    LOOP AT lt_t100 INTO t100.

      temp73-arbgb = t100-arbgb.
      temp73-msgnr = t100-msgnr.
      temp73-text = t100-text.
      INSERT temp73 INTO TABLE temp72.
    ENDLOOP.
    mt_result = temp72.

    IF mt_result IS INITIAL.
      client->message_toast_display( `No messages found.` ).
      RETURN.
    ENDIF.

    read_details( ).

    apply_min_languages( ).

  ENDMETHOD.

  METHOD read_details.

    TYPES:
      BEGIN OF ty_s_count,
        arbgb TYPE c LENGTH 20,
        msgnr TYPE c LENGTH 3,
        cnt   TYPE i,
      END OF ty_s_count.
    TYPES temp3 TYPE STANDARD TABLE OF ty_s_count WITH DEFAULT KEY.
DATA lt_count   TYPE temp3.
    DATA lt_text2   TYPE ty_t_t100.
    TYPES temp4 TYPE RANGE OF ty_s_count-arbgb.
DATA lt_r_arbgb TYPE temp4.
    TYPES temp5 TYPE RANGE OF ty_s_count-msgnr.
DATA lt_r_msgnr TYPE temp5.

    " ranges of the found classes and numbers instead of FOR ALL ENTRIES;
    " they select a superset, the exact pairs are matched below
    DATA temp16 LIKE lt_r_arbgb.
    DATA temp76 LIKE LINE OF mt_result.
      DATA temp17 LIKE LINE OF temp16.
    DATA temp18 LIKE lt_r_msgnr.
    DATA row LIKE LINE OF mt_result.
      DATA temp19 LIKE LINE OF temp18.
    DATA temp20 TYPE sy-langu.
    DATA lv_langu2 LIKE temp20.
    DATA lv_tabname TYPE string.
        DATA lx TYPE REF TO cx_root.
    DATA temp21 LIKE LINE OF mt_result.
    DATA lr_row LIKE REF TO temp21.
      DATA temp22 TYPE z2ui5_cl_popup_msg_search=>ty_s_row-text2.
      DATA temp23 TYPE z2ui5_cl_popup_msg_search=>ty_s_t100.
      DATA temp24 TYPE z2ui5_cl_popup_msg_search=>ty_s_row-count.
      DATA temp25 TYPE ty_s_count.
      DATA lv_count TYPE i.
      DATA lv_with TYPE string.
    CLEAR temp16.

    LOOP AT mt_result INTO temp76.

      temp17-sign = `I`.
      temp17-option = `EQ`.
      temp17-low = temp76-arbgb.
      INSERT temp17 INTO TABLE temp16.
    ENDLOOP.
    lt_r_arbgb = temp16.
    SORT lt_r_arbgb BY low.
    DELETE ADJACENT DUPLICATES FROM lt_r_arbgb COMPARING low.

    CLEAR temp18.

    LOOP AT mt_result INTO row.

      temp19-sign = `I`.
      temp19-option = `EQ`.
      temp19-low = row-msgnr.
      INSERT temp19 INTO TABLE temp18.
    ENDLOOP.
    lt_r_msgnr = temp18.
    SORT lt_r_msgnr BY low.
    DELETE ADJACENT DUPLICATES FROM lt_r_msgnr COMPARING low.
    IF lt_r_arbgb IS INITIAL OR lt_r_msgnr IS INITIAL.
      RETURN.
    ENDIF.


    temp20 = mv_langu2.

    lv_langu2 = temp20.

    lv_tabname = `T100`.

    TRY.
        SELECT sprsl arbgb msgnr text
          FROM (lv_tabname) INTO CORRESPONDING FIELDS OF TABLE lt_text2
          WHERE sprsl = lv_langu2
            AND arbgb IN lt_r_arbgb
            AND msgnr IN lt_r_msgnr
          ORDER BY arbgb msgnr
          .

        SELECT arbgb msgnr COUNT( * ) AS cnt
          FROM (lv_tabname) INTO CORRESPONDING FIELDS OF TABLE lt_count
          WHERE arbgb IN lt_r_arbgb
            AND msgnr IN lt_r_msgnr
          GROUP BY arbgb, msgnr
          ORDER BY arbgb msgnr
          .

      CATCH cx_root INTO lx.
        client->message_toast_display( |Details could not be read: { lx->get_text( ) }| ).
    ENDTRY.



    LOOP AT mt_result REFERENCE INTO lr_row.


      CLEAR temp22.

      READ TABLE lt_text2 INTO temp23 WITH KEY arbgb = lr_row->arbgb msgnr = lr_row->msgnr.
      IF sy-subrc = 0.
        temp22 = temp23-text.
      ENDIF.
      lr_row->text2 = temp22.

      CLEAR temp24.

      READ TABLE lt_count INTO temp25 WITH KEY arbgb = lr_row->arbgb msgnr = lr_row->msgnr.
      IF sy-subrc = 0.
        temp24 = temp25-cnt.
      ENDIF.
      lr_row->count = temp24.

      " the statement that raises the message, with a WITH for each placeholder

      lv_count = count( val = lr_row->text sub = `&` ).

      lv_with = ``.
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
    result = replace( val = result sub = `#` with = `##` occ = 0 ).
    result = replace( val = result sub = `%` with = `#%` occ = 0 ).
    result = replace( val = result sub = `_` with = `#_` occ = 0 ).
    result = replace( val = result sub = `*` with = `%` occ = 0 ).

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
    TYPES temp6 TYPE STANDARD TABLE OF ty_s_col WITH DEFAULT KEY.
DATA lt_col TYPE temp6.
    DATA popup   TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA dialog  TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA content TYPE REF TO z2ui5_cl_ui5_view_builder.

    DATA embedded TYPE abap_bool.
    DATA temp1 TYPE xsdboolean.
    DATA temp2 TYPE xsdboolean.
    DATA temp4 TYPE xsdboolean.
    DATA temp5 TYPE xsdboolean.
    DATA temp7 TYPE xsdboolean.
    DATA temp8 TYPE xsdboolean.
    DATA temp9 TYPE xsdboolean.
    DATA temp26 TYPE string.
    DATA table TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA temp27 LIKE lt_col.
    DATA temp28 LIKE LINE OF temp27.
    DATA columns TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA cells TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA ls_col LIKE LINE OF lt_col.
    DATA temp3 TYPE xsdboolean.
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

    temp4 = boolc( mv_check_disabled = abap_false ).

    temp5 = boolc( mv_check_disabled = abap_false ).

    temp7 = boolc( mv_check_disabled = abap_false ).

    temp8 = boolc( mv_check_disabled = abap_false ).

    temp9 = boolc( mv_check_disabled = abap_false ).
    content->ele( `HBox`
        )->a( n = `alignItems` v = `Center`
        )->a( n = `wrap` v = `Wrap`
        )->tag( `SearchField`
        )->a( n = `value` v = client->_bind( mv_search )
        )->a( n = `search` v = client->_event( `SEARCH` )
        )->a( n = `placeholder` v = `Message text, * as wildcard`
        )->a( n = `enabled` b = temp2
        )->a( n = `width` v = `25rem`
        )->tag( `Label`
        )->a( n = `text` v = `Message Class`
        )->a( n = `class` v = `sapUiSmallMarginBegin sapUiTinyMarginEnd`
        )->tag( `Input`
        )->a( n = `value` v = client->_bind( mv_msgid )
        )->a( n = `submit` v = client->_event( `SEARCH` )
        )->a( n = `enabled` b = temp4
        )->a( n = `width` v = `12rem`
        )->tag( `Label`
        )->a( n = `text` v = `Number`
        )->a( n = `class` v = `sapUiSmallMarginBegin sapUiTinyMarginEnd`
        )->tag( `Input`
        )->a( n = `value` v = client->_bind( mv_msgno )
        )->a( n = `submit` v = client->_event( `SEARCH` )
        )->a( n = `enabled` b = temp5
        )->a( n = `width` v = `5rem`
        )->tag( `Label`
        )->a( n = `text` v = `Language`
        )->a( n = `class` v = `sapUiSmallMarginBegin sapUiTinyMarginEnd`
        )->ele( `Select`
        )->a( n = `selectedKey` v = client->_bind( mv_langu )
        )->a( n = `items` v = client->_bind( mt_langu )
        )->a( n = `change` v = client->_event( `SEARCH` )
        )->a( n = `enabled` b = temp7
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
        )->a( n = `enabled` b = temp8
        )->tag( n = `Item` ns = `core`
        )->a( n = `key` v = `{KEY}`
        )->a( n = `text` v = `{TEXT}`
        )->end(
        )->tag( `Button`
        )->a( n = `text` v = `Search`
        )->a( n = `icon` v = `sap-icon://search`
        )->a( n = `press` v = client->_event( `SEARCH` )
        )->a( n = `enabled` b = temp9
        )->a( n = `class` v = `sapUiSmallMarginBegin` ).


    IF embedded = abap_true.
      temp26 = `None`.
    ELSE.
      temp26 = `SingleSelectLeft`.
    ENDIF.

    table = content->ele( `Table`
                             )->a( n = `mode` v = temp26
                             )->a( n = `items` v = client->_bind( mt_result )
                             )->a( n = `growing` b = abap_true
                             )->a( n = `growingThreshold` v = `50`
                             )->a( n = `noDataText` v = `Enter a text, a message class or a number and press Enter` ).


    CLEAR temp27.

    CLEAR temp28.
    temp28-name = `ARBGB`.
    temp28-text = `Message Class`.
    temp28-width = `12rem`.
    INSERT temp28 INTO TABLE temp27.
    CLEAR temp28.
    temp28-name = `MSGNR`.
    temp28-text = `Number`.
    temp28-width = `5rem`.
    INSERT temp28 INTO TABLE temp27.
    CLEAR temp28.
    temp28-name = `COUNT`.
    temp28-text = `Languages`.
    temp28-width = `6rem`.
    INSERT temp28 INTO TABLE temp27.
    CLEAR temp28.
    temp28-name = `TEXT`.
    temp28-text = `Text`.
    INSERT temp28 INTO TABLE temp27.
    CLEAR temp28.
    temp28-name = `TEXT2`.
    temp28-text = `Text (2nd Language)`.
    INSERT temp28 INTO TABLE temp27.
    CLEAR temp28.
    temp28-name = `STATEMENT`.
    temp28-text = `Statement`.
    INSERT temp28 INTO TABLE temp27.
    lt_col = temp27.


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
          )->a( n = `text` v = |\{{ ls_col-name }\}| ).
    ENDLOOP.

    IF embedded = abap_true.
      RETURN.
    ENDIF.


    temp3 = boolc( mv_check_disabled = abap_false ).
    dialog->ele( `buttons`
        )->tag( `Button`
        )->a( n = `text` v = `Cancel`
        )->a( n = `press` v = client->_event( `CANCEL` )
        )->tag( `Button`
        )->a( n = `text` v = `OK`
        )->a( n = `press` v = client->_event( `CONFIRM` )
        )->a( n = `enabled` b = temp3
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

    " no single value - empty
    CLEAR result.

  ENDMETHOD.

ENDCLASS.
