CLASS ltcl_display DEFINITION DEFERRED.
CLASS z2ui5_cl_popup_msg_search DEFINITION LOCAL FRIENDS ltcl_display.

CLASS ltcl_popup_result DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA mo_action TYPE REF TO z2ui5_cl_ui5_action.

    METHODS roundtrip
      IMPORTING
        io_app   TYPE REF TO z2ui5_if_app
        iv_event TYPE string
        it_arg   TYPE string_table OPTIONAL.

    METHODS test_initial_not_confirmed FOR TESTING RAISING cx_static_check.
    METHODS test_confirmed             FOR TESTING RAISING cx_static_check.
    METHODS test_cancelled             FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_popup_result IMPLEMENTATION.

  METHOD roundtrip.

    " a roundtrip with an event, after the first one - as the button press
    " of the user
    DATA temp2 TYPE REF TO z2ui5_cl_ui5_handler.
    DATA temp1 TYPE REF TO z2ui5_cl_ui5_client.
    CREATE OBJECT temp2 TYPE z2ui5_cl_ui5_handler EXPORTING VAL = ``.
    CREATE OBJECT mo_action EXPORTING VAL = temp2.
    mo_action->mo_app->mo_app = io_app.
    mo_action->mo_app->mv_check_initialized = abap_true.
    mo_action->ms_actual-event = iv_event.
    mo_action->ms_actual-t_event_arg = it_arg.

    CREATE OBJECT temp1 TYPE z2ui5_cl_ui5_client EXPORTING ACTION = mo_action.
    io_app->main( temp1 ).

  ENDMETHOD.

  METHOD test_initial_not_confirmed.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_msg_search.
    DATA temp2 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp2.
    lo_pop = z2ui5_cl_popup_msg_search=>factory( ).

    temp2 ?= lo_pop.

    li_result = temp2.

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

  METHOD test_confirmed.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_msg_search.
    DATA temp3 TYPE z2ui5_cl_popup_msg_search=>ty_t_row.
    DATA temp4 LIKE LINE OF temp3.
    DATA temp5 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp5.
    DATA temp6 TYPE REF TO z2ui5_cl_popup_msg_search=>ty_s_result.
    DATA lr_result LIKE temp6.
    lo_pop = z2ui5_cl_popup_msg_search=>factory( ).

    CLEAR temp3.

    temp4-selkz = abap_true.
    temp4-arbgb = `00`.
    temp4-msgnr = `001`.
    temp4-text = `&1&2&3&4`.
    INSERT temp4 INTO TABLE temp3.
    lo_pop->mt_result = temp3.
    roundtrip( io_app   = lo_pop
               iv_event = `CONFIRM` ).

    temp5 ?= lo_pop.

    li_result = temp5.

    cl_abap_unit_assert=>assert_true( li_result->check_confirmed( ) ).

    temp6 ?= li_result->value( ).

    lr_result = temp6.
    cl_abap_unit_assert=>assert_equals( exp = `00`
                                        act = lr_result->msgid ).
    cl_abap_unit_assert=>assert_equals( exp = `001`
                                        act = lr_result->msgno ).
    cl_abap_unit_assert=>assert_initial( li_result->value_string( ) ).

  ENDMETHOD.

  METHOD test_cancelled.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_msg_search.
    DATA temp7 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp7.
    lo_pop = z2ui5_cl_popup_msg_search=>factory( ).
    roundtrip( io_app   = lo_pop
               iv_event = `CANCEL` ).

    temp7 ?= lo_pop.

    li_result = temp7.

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

ENDCLASS.


"! The view of both modes: the popup (unchanged, byte for byte the XML it
"! had before the embedded mode) and the content embedded in a host page.
CLASS ltcl_display DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA mo_action TYPE REF TO z2ui5_cl_ui5_action.
    DATA mo_pop    TYPE REF TO z2ui5_cl_popup_msg_search.

    METHODS setup.
    METHODS init_popup.

    METHODS front_xml
      IMPORTING
        iv_method     TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS test_popup_golden    FOR TESTING RAISING cx_static_check.
    METHODS test_embedded        FOR TESTING RAISING cx_static_check.
    METHODS test_embedded_events FOR TESTING RAISING cx_static_check.
    METHODS test_languages_installed FOR TESTING RAISING cx_static_check.
    METHODS test_languages_no_filter FOR TESTING RAISING cx_static_check.
    METHODS test_languages_no_texts  FOR TESTING RAISING cx_static_check.
    METHODS test_languages_filter    FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_display IMPLEMENTATION.

  METHOD setup.

    init_popup( ).

  ENDMETHOD.

  METHOD init_popup.

    DATA temp3 TYPE REF TO z2ui5_cl_ui5_handler.
    CREATE OBJECT temp3 TYPE z2ui5_cl_ui5_handler EXPORTING VAL = ``.
    CREATE OBJECT mo_action EXPORTING VAL = temp3.
    mo_pop = z2ui5_cl_popup_msg_search=>factory( ).
    mo_action->mo_app->mo_app = mo_pop.
    mo_action->mo_app->mv_check_initialized = abap_true.
    CREATE OBJECT mo_pop->client TYPE z2ui5_cl_ui5_client EXPORTING ACTION = mo_action.

  ENDMETHOD.

  METHOD front_xml.

    DATA ls_action LIKE LINE OF mo_action->ms_next-t_action_front.
    LOOP AT mo_action->ms_next-t_action_front INTO ls_action
         WHERE method = iv_method.
      result = |{ result }{ ls_action-slot }:{ ls_action-xml }|.
    ENDLOOP.

  ENDMETHOD.

  METHOD test_popup_golden.

    mo_pop->display( ).

    cl_abap_unit_assert=>assert_equals(
        exp = `POPUP:` &&
              `<core:FragmentDefinition xmlns="sap.m" xmlns:core="sap.ui.core"><Dialog title="Message Sea` &&
              `rch" contentWidth="90%" contentHeight="80%" afterClose=".eB(['CANCEL'])"><content><VBox cl` &&
              `ass="sapUiSmallMargin"><MessageStrip text="{/MV_INFO}" type="Warning" visible="{/MV_CHECK_` &&
              `INFO}"/><HBox alignItems="Center" wrap="Wrap"><SearchField value="{/MV_SEARCH}" search=".e` &&
              `B(['SEARCH'])" placeholder="Message text, * as wildcard" enabled="true" width="25rem"/><La` &&
              `bel text="Message Class" class="sapUiSmallMarginBegin sapUiTinyMarginEnd"/><Input value="{` &&
              `/MV_MSGID}" submit=".eB(['SEARCH'])" enabled="true" width="12rem"/><Label text="Number" cl` &&
              `ass="sapUiSmallMarginBegin sapUiTinyMarginEnd"/><Input value="{/MV_MSGNO}" submit=".eB(['S` &&
              `EARCH'])" enabled="true" width="5rem"/><Label text="Language" class="sapUiSmallMarginBegin` &&
              ` sapUiTinyMarginEnd"/><Select selectedKey="{/MV_LANGU}" items="{/MT_LANGU}" change=".eB(['` &&
              `SEARCH'])" enabled="true"><core:Item key="{KEY}" text="{TEXT}"/></Select><Label text="2nd ` &&
              `Language" class="sapUiSmallMarginBegin sapUiTinyMarginEnd"/><Select selectedKey="{/MV_LANG` &&
              `U2}" items="{/MT_LANGU}" change=".eB(['SEARCH'])" enabled="true"><core:Item key="{KEY}" te` &&
              `xt="{TEXT}"/></Select><Button text="Search" icon="sap-icon://search" press=".eB(['SEARCH']` &&
              `)" enabled="true" class="sapUiSmallMarginBegin"/></HBox><Table mode="SingleSelectLeft" ite` &&
              `ms="{/MT_RESULT}" growing="true" growingThreshold="50" noDataText="Enter a text, a message` &&
              ` class or a number and press Enter"><columns><Column width="12rem"><Text text="Message Cla` &&
              `ss"/></Column><Column width="5rem"><Text text="Number"/></Column><Column width="6rem"><Tex` &&
              `t text="Languages"/></Column><Column width=""><Text text="Text"/></Column><Column width=""` &&
              `><Text text="Text (2nd Language)"/></Column><Column width=""><Text text="Statement"/></Col` &&
              `umn></columns><items><ColumnListItem selected="{SELKZ}"><cells><Text text="{ARBGB}"/><Text` &&
              ` text="{MSGNR}"/><Text text="{COUNT}"/><Text text="{TEXT}"/><Text text="{TEXT2}"/><Text te` &&
              `xt="{STATEMENT}"/></cells></ColumnListItem></items></Table></VBox></content><buttons><Butt` &&
              `on text="Cancel" press=".eB(['CANCEL'])"/><Button text="OK" press=".eB(['CONFIRM'])" enabl` &&
              `ed="true" type="Emphasized"/></buttons></Dialog></core:FragmentDefinition>`
        act = front_xml( `display` ) ).

  ENDMETHOD.

  METHOD test_embedded.

    DATA lo_page TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA temp8 TYPE REF TO z2ui5_if_app.
    DATA lv_xml TYPE string.
    lo_page = z2ui5_cl_ui5_view_builder=>factory( )->ele( `Page` ).
    mo_pop->z2ui5_if_popup_embed~mo_parent_view = lo_page.


    temp8 ?= mo_pop.
    temp8->main( mo_pop->client ).

    " no popup, no navigation - the host displays its page
    cl_abap_unit_assert=>assert_initial( front_xml( `display` ) ).
    cl_abap_unit_assert=>assert_not_bound( mo_action->ms_next-o_app_leave ).
    cl_abap_unit_assert=>assert_true( mo_pop->z2ui5_if_popup_embed~mv_view_display ).


    lv_xml = lo_page->stringify( ).
    cl_abap_unit_assert=>assert_char_cp( act = lv_xml
                                         exp = `<Page><VBox class="sapUiSmallMargin"><MessageStrip *` ).
    cl_abap_unit_assert=>assert_char_cp( act = lv_xml
                                         exp = `*<Table mode="None" items="{/MT_RESULT}"*` ).
    cl_abap_unit_assert=>assert_char_np( act = lv_xml
                                         exp = `*Dialog*` ).
    cl_abap_unit_assert=>assert_char_np( act = lv_xml
                                         exp = `*CONFIRM*` ).

  ENDMETHOD.

  METHOD test_embedded_events.
    DATA temp9 TYPE REF TO z2ui5_if_app.
    DATA temp4 TYPE string_table.
    DATA temp10 LIKE temp4.
    DATA lv_event LIKE LINE OF temp10.
      DATA temp11 TYPE REF TO z2ui5_if_app.
      DATA temp12 TYPE REF TO z2ui5_if_popup_result.

    mo_pop->z2ui5_if_popup_embed~mo_parent_view = z2ui5_cl_ui5_view_builder=>factory( )->ele( `Page` ).

    temp9 ?= mo_pop.
    temp9->main( mo_pop->client ).
    CLEAR mo_pop->z2ui5_if_popup_embed~mv_view_display.

    " a later roundtrip with an event of the host, then CANCEL and CONFIRM:
    " embedded, the app never leaves and never asks for a new display

    CLEAR temp4.
    INSERT `HOST_EVENT` INTO TABLE temp4.
    INSERT `CANCEL` INTO TABLE temp4.
    INSERT `CONFIRM` INTO TABLE temp4.

    temp10 = temp4.

    LOOP AT temp10 INTO lv_event.
      init_popup( ).
      mo_pop->z2ui5_if_popup_embed~mo_parent_view = z2ui5_cl_ui5_view_builder=>factory( )->ele( `Page` ).
      mo_pop->embed_initialized = abap_true.
      mo_action->ms_actual-event = lv_event.

      temp11 ?= mo_pop.
      temp11->main( mo_pop->client ).
      cl_abap_unit_assert=>assert_not_bound( mo_action->ms_next-o_app_leave ).
      cl_abap_unit_assert=>assert_false( mo_pop->z2ui5_if_popup_embed~mv_view_display ).

      temp12 ?= mo_pop.
      cl_abap_unit_assert=>assert_false( temp12->check_confirmed( ) ).
      cl_abap_unit_assert=>assert_initial( front_xml( `display` ) ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_languages_installed.

    " T002C flags other languages only (seen on a system): English and
    " German are offered all the same, and first
    DATA temp13 TYPE z2ui5_cl_popup_msg_search=>ty_t_t002t.
    DATA temp14 LIKE LINE OF temp13.
    DATA temp6 TYPE string_table.
    DATA lt_langu TYPE z2ui5_cl_popup_msg_search=>ty_t_langu.
    FIELD-SYMBOLS <temp15> LIKE LINE OF lt_langu.
    DATA temp16 LIKE sy-tabix.
    FIELD-SYMBOLS <temp17> LIKE LINE OF lt_langu.
    DATA temp18 LIKE sy-tabix.
    FIELD-SYMBOLS <temp19> LIKE LINE OF lt_langu.
    DATA temp20 LIKE sy-tabix.
    FIELD-SYMBOLS <temp21> LIKE LINE OF lt_langu.
    DATA temp22 LIKE sy-tabix.
    FIELD-SYMBOLS <temp23> LIKE LINE OF lt_langu.
    DATA temp24 LIKE sy-tabix.
      DATA temp1 TYPE xsdboolean.
      DATA temp2 LIKE sy-subrc.
    CLEAR temp13.

    temp14-sprsl = `A`.
    temp14-sptxt = `Albanian`.
    INSERT temp14 INTO TABLE temp13.
    temp14-sprsl = `D`.
    temp14-sptxt = `German`.
    INSERT temp14 INTO TABLE temp13.
    temp14-sprsl = `E`.
    temp14-sptxt = `English`.
    INSERT temp14 INTO TABLE temp13.
    temp14-sprsl = `P`.
    temp14-sptxt = `Portuguese`.
    INSERT temp14 INTO TABLE temp13.
    temp14-sprsl = `Q`.
    temp14-sptxt = `Other`.
    INSERT temp14 INTO TABLE temp13.

    CLEAR temp6.
    INSERT `P` INTO TABLE temp6.
    INSERT `A` INTO TABLE temp6.

    lt_langu = mo_pop->get_language_list(
        t002t     = temp13
        installed = temp6 ).



    temp16 = sy-tabix.
    READ TABLE lt_langu INDEX 1 ASSIGNING <temp15>.
    sy-tabix = temp16.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `E`
                                        act = <temp15>-key ).


    temp18 = sy-tabix.
    READ TABLE lt_langu INDEX 1 ASSIGNING <temp17>.
    sy-tabix = temp18.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `English (E)`
                                        act = <temp17>-text ).


    temp20 = sy-tabix.
    READ TABLE lt_langu INDEX 2 ASSIGNING <temp19>.
    sy-tabix = temp20.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `D`
                                        act = <temp19>-key ).


    temp22 = sy-tabix.
    READ TABLE lt_langu INDEX 3 ASSIGNING <temp21>.
    sy-tabix = temp22.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A`
                                        act = <temp21>-key ).


    temp24 = sy-tabix.
    READ TABLE lt_langu INDEX 4 ASSIGNING <temp23>.
    sy-tabix = temp24.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `P`
                                        act = <temp23>-key ).
    IF sy-langu <> `Q`.


      READ TABLE lt_langu WITH KEY key = `Q` TRANSPORTING NO FIELDS.
      temp2 = sy-subrc.
      temp1 = boolc( temp2 = 0 ).
      cl_abap_unit_assert=>assert_false( temp1 ).
    ENDIF.

  ENDMETHOD.

  METHOD test_languages_no_filter.

    " T002C not readable: every language of T002T, English and German first
    DATA temp25 TYPE z2ui5_cl_popup_msg_search=>ty_t_t002t.
    DATA temp26 LIKE LINE OF temp25.
    DATA temp8 TYPE string_table.
    DATA lt_langu TYPE z2ui5_cl_popup_msg_search=>ty_t_langu.
    FIELD-SYMBOLS <temp27> LIKE LINE OF lt_langu.
    DATA temp28 LIKE sy-tabix.
    FIELD-SYMBOLS <temp29> LIKE LINE OF lt_langu.
    DATA temp30 LIKE sy-tabix.
    FIELD-SYMBOLS <temp31> LIKE LINE OF lt_langu.
    DATA temp32 LIKE sy-tabix.
    FIELD-SYMBOLS <temp33> LIKE LINE OF lt_langu.
    DATA temp34 LIKE sy-tabix.
    CLEAR temp25.

    temp26-sprsl = `A`.
    temp26-sptxt = `Albanian`.
    INSERT temp26 INTO TABLE temp25.
    temp26-sprsl = `D`.
    temp26-sptxt = `German`.
    INSERT temp26 INTO TABLE temp25.
    temp26-sprsl = `E`.
    temp26-sptxt = `English`.
    INSERT temp26 INTO TABLE temp25.
    temp26-sprsl = `Q`.
    temp26-sptxt = `Other`.
    INSERT temp26 INTO TABLE temp25.

    CLEAR temp8.

    lt_langu = mo_pop->get_language_list(
        t002t     = temp25
        installed = temp8 ).



    temp28 = sy-tabix.
    READ TABLE lt_langu INDEX 1 ASSIGNING <temp27>.
    sy-tabix = temp28.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `E`
                                        act = <temp27>-key ).


    temp30 = sy-tabix.
    READ TABLE lt_langu INDEX 2 ASSIGNING <temp29>.
    sy-tabix = temp30.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `D`
                                        act = <temp29>-key ).


    temp32 = sy-tabix.
    READ TABLE lt_langu INDEX 3 ASSIGNING <temp31>.
    sy-tabix = temp32.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A`
                                        act = <temp31>-key ).


    temp34 = sy-tabix.
    READ TABLE lt_langu INDEX 4 ASSIGNING <temp33>.
    sy-tabix = temp34.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `Q`
                                        act = <temp33>-key ).

  ENDMETHOD.

  METHOD test_languages_no_texts.
    DATA temp35 TYPE z2ui5_cl_popup_msg_search=>ty_t_t002t.
    DATA temp9 TYPE string_table.
    DATA lt_langu TYPE z2ui5_cl_popup_msg_search=>ty_t_langu.
    FIELD-SYMBOLS <temp36> LIKE LINE OF lt_langu.
    DATA temp37 LIKE sy-tabix.
    FIELD-SYMBOLS <temp38> LIKE LINE OF lt_langu.
    DATA temp39 LIKE sy-tabix.
    DATA temp2 TYPE xsdboolean.
    DATA temp3 LIKE sy-subrc.

    " neither T002T nor T002C readable: the selected languages still have an
    " entry, so selectedKey always matches
    mo_pop->mv_langu  = `F`.
    mo_pop->mv_langu2 = `D`.

    CLEAR temp35.

    CLEAR temp9.

    lt_langu = mo_pop->get_language_list( t002t     = temp35
                                                installed = temp9 ).



    temp37 = sy-tabix.
    READ TABLE lt_langu INDEX 1 ASSIGNING <temp36>.
    sy-tabix = temp37.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `English (E)`
                                        act = <temp36>-text ).


    temp39 = sy-tabix.
    READ TABLE lt_langu INDEX 2 ASSIGNING <temp38>.
    sy-tabix = temp39.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `German (D)`
                                        act = <temp38>-text ).


    READ TABLE lt_langu WITH KEY key = `F` TRANSPORTING NO FIELDS.
    temp3 = sy-subrc.
    temp2 = boolc( temp3 = 0 ).
    cl_abap_unit_assert=>assert_true( temp2 ).

  ENDMETHOD.

  METHOD test_languages_filter.
    DATA temp40 TYPE z2ui5_cl_popup_msg_search=>ty_t_t002t.
    DATA temp41 LIKE LINE OF temp40.
    DATA temp10 TYPE string_table.
    DATA lt_langu TYPE z2ui5_cl_popup_msg_search=>ty_t_langu.
    FIELD-SYMBOLS <temp42> LIKE LINE OF lt_langu.
    DATA temp43 LIKE sy-tabix.
    FIELD-SYMBOLS <temp44> LIKE LINE OF lt_langu.
    DATA temp45 LIKE sy-tabix.
    FIELD-SYMBOLS <temp46> LIKE LINE OF lt_langu.
    DATA temp47 LIKE sy-tabix.

    " T002C flags D, E and F: exactly those stay - the filter of 9383ad7
    " (DELETE TABLE ... FROM on a table WITH EMPTY KEY) removed the wrong rows
    IF sy-langu <> `E` AND sy-langu <> `D` AND sy-langu <> `F`.
      RETURN.
    ENDIF.

    CLEAR temp40.

    temp41-sprsl = `A`.
    temp41-sptxt = `Albanian`.
    INSERT temp41 INTO TABLE temp40.
    temp41-sprsl = `D`.
    temp41-sptxt = `German`.
    INSERT temp41 INTO TABLE temp40.
    temp41-sprsl = `E`.
    temp41-sptxt = `English`.
    INSERT temp41 INTO TABLE temp40.
    temp41-sprsl = `F`.
    temp41-sptxt = `French`.
    INSERT temp41 INTO TABLE temp40.
    temp41-sprsl = `H`.
    temp41-sptxt = `Hungarian`.
    INSERT temp41 INTO TABLE temp40.
    temp41-sprsl = `Q`.
    temp41-sptxt = `Other`.
    INSERT temp41 INTO TABLE temp40.

    CLEAR temp10.
    INSERT `D` INTO TABLE temp10.
    INSERT `E` INTO TABLE temp10.
    INSERT `F` INTO TABLE temp10.

    lt_langu = mo_pop->get_language_list(
        t002t     = temp40
        installed = temp10 ).

    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lt_langu ) ).


    temp43 = sy-tabix.
    READ TABLE lt_langu INDEX 1 ASSIGNING <temp42>.
    sy-tabix = temp43.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `E`
                                        act = <temp42>-key ).


    temp45 = sy-tabix.
    READ TABLE lt_langu INDEX 2 ASSIGNING <temp44>.
    sy-tabix = temp45.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `D`
                                        act = <temp44>-key ).


    temp47 = sy-tabix.
    READ TABLE lt_langu INDEX 3 ASSIGNING <temp46>.
    sy-tabix = temp47.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `French (F)`
                                        act = <temp46>-text ).

  ENDMETHOD.

ENDCLASS.
