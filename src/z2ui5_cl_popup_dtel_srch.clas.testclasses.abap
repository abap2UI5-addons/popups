CLASS ltcl_display DEFINITION DEFERRED.
CLASS z2ui5_cl_popup_dtel_srch DEFINITION LOCAL FRIENDS ltcl_display.

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
    METHODS test_confirm_without_row   FOR TESTING RAISING cx_static_check.

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

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_dtel_srch.
    DATA temp2 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp2.
    lo_pop = z2ui5_cl_popup_dtel_srch=>factory( ).

    temp2 ?= lo_pop.

    li_result = temp2.

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

  METHOD test_confirmed.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_dtel_srch.
    DATA temp3 TYPE z2ui5_cl_popup_dtel_srch=>ty_t_row.
    DATA temp4 LIKE LINE OF temp3.
    DATA temp5 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp5.
    DATA temp6 TYPE REF TO z2ui5_cl_popup_dtel_srch=>ty_s_result.
    DATA lr_value LIKE temp6.
    lo_pop = z2ui5_cl_popup_dtel_srch=>factory( ).

    CLEAR temp3.

    CLEAR temp4.
    temp4-rollname = `MARA`.
    INSERT temp4 INTO TABLE temp3.
    CLEAR temp4.
    temp4-selkz = abap_true.
    temp4-rollname = `MATNR`.
    temp4-description = `Material`.
    INSERT temp4 INTO TABLE temp3.
    lo_pop->mt_result = temp3.
    roundtrip( io_app   = lo_pop
               iv_event = `CONFIRM` ).

    temp5 ?= lo_pop.

    li_result = temp5.

    cl_abap_unit_assert=>assert_true( li_result->check_confirmed( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `MATNR`
                                        act = li_result->value_string( ) ).

    temp6 ?= li_result->value( ).

    lr_value = temp6.
    cl_abap_unit_assert=>assert_equals( exp = `Material`
                                        act = lr_value->description ).

  ENDMETHOD.

  METHOD test_cancelled.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_dtel_srch.
    DATA temp7 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp7.
    lo_pop = z2ui5_cl_popup_dtel_srch=>factory( ).
    roundtrip( io_app   = lo_pop
               iv_event = `CANCEL` ).

    temp7 ?= lo_pop.

    li_result = temp7.

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

  METHOD test_confirm_without_row.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_dtel_srch.
    DATA temp8 TYPE z2ui5_cl_popup_dtel_srch=>ty_t_row.
    DATA temp9 LIKE LINE OF temp8.
    DATA temp10 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp10.
    lo_pop = z2ui5_cl_popup_dtel_srch=>factory( ).

    CLEAR temp8.

    temp9-rollname = `MATNR`.
    INSERT temp9 INTO TABLE temp8.
    lo_pop->mt_result = temp8.
    roundtrip( io_app   = lo_pop
               iv_event = `CONFIRM` ).

    temp10 ?= lo_pop.

    li_result = temp10.

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

ENDCLASS.


"! The view of both modes: the popup (unchanged, byte for byte the XML it
"! had before the embedded mode) and the content embedded in a host page.
CLASS ltcl_display DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA mo_action TYPE REF TO z2ui5_cl_ui5_action.
    DATA mo_pop    TYPE REF TO z2ui5_cl_popup_dtel_srch.

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
    mo_pop = z2ui5_cl_popup_dtel_srch=>factory( ).
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
              `<core:FragmentDefinition xmlns="sap.m" xmlns:core="sap.ui.core"><Dialog title="Data Elemen` &&
              `t Search" contentWidth="90%" contentHeight="80%" afterClose=".eB(['CANCEL'])"><content><VB` &&
              `ox class="sapUiSmallMargin"><MessageStrip text="{/MV_INFO}" type="Warning" visible="{/MV_C` &&
              `HECK_INFO}"/><HBox alignItems="Center" wrap="Wrap"><SearchField value="{/MV_SEARCH}" searc` &&
              `h=".eB(['SEARCH'])" placeholder="Text or name of the data element, * as wildcard" enabled=` &&
              `"true" width="30rem"/><Label text="Language" class="sapUiSmallMarginBegin sapUiTinyMarginE` &&
              `nd"/><Select selectedKey="{/MV_LANGU}" items="{/MT_LANGU}" change=".eB(['SEARCH'])" enable` &&
              `d="true"><core:Item key="{KEY}" text="{TEXT}"/></Select><Label text="2nd Language" class="` &&
              `sapUiSmallMarginBegin sapUiTinyMarginEnd"/><Select selectedKey="{/MV_LANGU2}" items="{/MT_` &&
              `LANGU}" change=".eB(['SEARCH'])" enabled="true"><core:Item key="{KEY}" text="{TEXT}"/></Se` &&
              `lect></HBox><Table mode="SingleSelectLeft" items="{/MT_RESULT}" growing="true" growingThre` &&
              `shold="50" noDataText="Enter a search text and press Enter"><columns><Column width="14rem"` &&
              `><Text text="Data Element"/></Column><Column width="5rem"><Text text="Type"/></Column><Col` &&
              `umn width="5rem"><Text text="Length"/></Column><Column width="6rem"><Text text="Languages"` &&
              `/></Column><Column width=""><Text text="Description"/></Column><Column width=""><Text text` &&
              `="Short"/></Column><Column width=""><Text text="Medium"/></Column><Column width=""><Text t` &&
              `ext="Long"/></Column><Column width=""><Text text="Heading"/></Column><Column width=""><Tex` &&
              `t text="Description (2nd Language)"/></Column><Column width=""><Text text="Long (2nd Langu` &&
              `age)"/></Column></columns><items><ColumnListItem selected="{SELKZ}"><cells><Text text="{RO` &&
              `LLNAME}"/><Text text="{DATATYPE}"/><Text text="{LENG}"/><Text text="{COUNT}"/><Text text="` &&
              `{DESCRIPTION}"/><Text text="{SHORT}"/><Text text="{MEDIUM}"/><Text text="{LONG}"/><Text te` &&
              `xt="{HEADING}"/><Text text="{DESCRIPTION2}"/><Text text="{LONG2}"/></cells></ColumnListIte` &&
              `m></items></Table></VBox></content><buttons><Button text="Cancel" press=".eB(['CANCEL'])"/` &&
              `><Button text="OK" press=".eB(['CONFIRM'])" enabled="true" type="Emphasized"/></buttons></` &&
              `Dialog></core:FragmentDefinition>`
        act = front_xml( `display` ) ).

  ENDMETHOD.

  METHOD test_embedded.

    DATA lo_page TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA temp11 TYPE REF TO z2ui5_if_app.
    DATA lv_xml TYPE string.
    lo_page = z2ui5_cl_ui5_view_builder=>factory( )->ele( `Page` ).
    mo_pop->z2ui5_if_popup_embed~mo_parent_view = lo_page.


    temp11 ?= mo_pop.
    temp11->main( mo_pop->client ).

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
    DATA temp12 TYPE REF TO z2ui5_if_app.
    DATA temp4 TYPE string_table.
    DATA temp13 LIKE temp4.
    DATA lv_event LIKE LINE OF temp13.
      DATA temp14 TYPE REF TO z2ui5_if_app.
      DATA temp15 TYPE REF TO z2ui5_if_popup_result.

    mo_pop->z2ui5_if_popup_embed~mo_parent_view = z2ui5_cl_ui5_view_builder=>factory( )->ele( `Page` ).

    temp12 ?= mo_pop.
    temp12->main( mo_pop->client ).
    CLEAR mo_pop->z2ui5_if_popup_embed~mv_view_display.

    " a later roundtrip with an event of the host, then CANCEL and CONFIRM:
    " embedded, the app never leaves and never asks for a new display

    CLEAR temp4.
    INSERT `HOST_EVENT` INTO TABLE temp4.
    INSERT `CANCEL` INTO TABLE temp4.
    INSERT `CONFIRM` INTO TABLE temp4.

    temp13 = temp4.

    LOOP AT temp13 INTO lv_event.
      init_popup( ).
      mo_pop->z2ui5_if_popup_embed~mo_parent_view = z2ui5_cl_ui5_view_builder=>factory( )->ele( `Page` ).
      mo_pop->embed_initialized = abap_true.
      mo_action->ms_actual-event = lv_event.

      temp14 ?= mo_pop.
      temp14->main( mo_pop->client ).
      cl_abap_unit_assert=>assert_not_bound( mo_action->ms_next-o_app_leave ).
      cl_abap_unit_assert=>assert_false( mo_pop->z2ui5_if_popup_embed~mv_view_display ).

      temp15 ?= mo_pop.
      cl_abap_unit_assert=>assert_false( temp15->check_confirmed( ) ).
      cl_abap_unit_assert=>assert_initial( front_xml( `display` ) ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_languages_installed.

    " T002C flags other languages only (seen on a system): English and
    " German are offered all the same, and first
    DATA temp16 TYPE z2ui5_cl_popup_dtel_srch=>ty_t_t002t.
    DATA temp17 LIKE LINE OF temp16.
    DATA temp6 TYPE string_table.
    DATA lt_langu TYPE z2ui5_cl_popup_dtel_srch=>ty_t_langu.
    FIELD-SYMBOLS <temp18> LIKE LINE OF lt_langu.
    DATA temp19 LIKE sy-tabix.
    FIELD-SYMBOLS <temp20> LIKE LINE OF lt_langu.
    DATA temp21 LIKE sy-tabix.
    FIELD-SYMBOLS <temp22> LIKE LINE OF lt_langu.
    DATA temp23 LIKE sy-tabix.
    FIELD-SYMBOLS <temp24> LIKE LINE OF lt_langu.
    DATA temp25 LIKE sy-tabix.
    FIELD-SYMBOLS <temp26> LIKE LINE OF lt_langu.
    DATA temp27 LIKE sy-tabix.
      DATA temp1 TYPE xsdboolean.
      DATA temp2 LIKE sy-subrc.
    CLEAR temp16.

    temp17-sprsl = `A`.
    temp17-sptxt = `Albanian`.
    INSERT temp17 INTO TABLE temp16.
    temp17-sprsl = `D`.
    temp17-sptxt = `German`.
    INSERT temp17 INTO TABLE temp16.
    temp17-sprsl = `E`.
    temp17-sptxt = `English`.
    INSERT temp17 INTO TABLE temp16.
    temp17-sprsl = `P`.
    temp17-sptxt = `Portuguese`.
    INSERT temp17 INTO TABLE temp16.
    temp17-sprsl = `Q`.
    temp17-sptxt = `Other`.
    INSERT temp17 INTO TABLE temp16.

    CLEAR temp6.
    INSERT `P` INTO TABLE temp6.
    INSERT `A` INTO TABLE temp6.

    lt_langu = mo_pop->get_language_list(
        t002t     = temp16
        installed = temp6 ).



    temp19 = sy-tabix.
    READ TABLE lt_langu INDEX 1 ASSIGNING <temp18>.
    sy-tabix = temp19.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `E`
                                        act = <temp18>-key ).


    temp21 = sy-tabix.
    READ TABLE lt_langu INDEX 1 ASSIGNING <temp20>.
    sy-tabix = temp21.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `English (E)`
                                        act = <temp20>-text ).


    temp23 = sy-tabix.
    READ TABLE lt_langu INDEX 2 ASSIGNING <temp22>.
    sy-tabix = temp23.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `D`
                                        act = <temp22>-key ).


    temp25 = sy-tabix.
    READ TABLE lt_langu INDEX 3 ASSIGNING <temp24>.
    sy-tabix = temp25.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A`
                                        act = <temp24>-key ).


    temp27 = sy-tabix.
    READ TABLE lt_langu INDEX 4 ASSIGNING <temp26>.
    sy-tabix = temp27.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `P`
                                        act = <temp26>-key ).
    IF sy-langu <> `Q`.


      READ TABLE lt_langu WITH KEY key = `Q` TRANSPORTING NO FIELDS.
      temp2 = sy-subrc.
      temp1 = boolc( temp2 = 0 ).
      cl_abap_unit_assert=>assert_false( temp1 ).
    ENDIF.

  ENDMETHOD.

  METHOD test_languages_no_filter.

    " T002C not readable: every language of T002T, English and German first
    DATA temp28 TYPE z2ui5_cl_popup_dtel_srch=>ty_t_t002t.
    DATA temp29 LIKE LINE OF temp28.
    DATA temp8 TYPE string_table.
    DATA lt_langu TYPE z2ui5_cl_popup_dtel_srch=>ty_t_langu.
    FIELD-SYMBOLS <temp30> LIKE LINE OF lt_langu.
    DATA temp31 LIKE sy-tabix.
    FIELD-SYMBOLS <temp32> LIKE LINE OF lt_langu.
    DATA temp33 LIKE sy-tabix.
    FIELD-SYMBOLS <temp34> LIKE LINE OF lt_langu.
    DATA temp35 LIKE sy-tabix.
    FIELD-SYMBOLS <temp36> LIKE LINE OF lt_langu.
    DATA temp37 LIKE sy-tabix.
    CLEAR temp28.

    temp29-sprsl = `A`.
    temp29-sptxt = `Albanian`.
    INSERT temp29 INTO TABLE temp28.
    temp29-sprsl = `D`.
    temp29-sptxt = `German`.
    INSERT temp29 INTO TABLE temp28.
    temp29-sprsl = `E`.
    temp29-sptxt = `English`.
    INSERT temp29 INTO TABLE temp28.
    temp29-sprsl = `Q`.
    temp29-sptxt = `Other`.
    INSERT temp29 INTO TABLE temp28.

    CLEAR temp8.

    lt_langu = mo_pop->get_language_list(
        t002t     = temp28
        installed = temp8 ).



    temp31 = sy-tabix.
    READ TABLE lt_langu INDEX 1 ASSIGNING <temp30>.
    sy-tabix = temp31.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `E`
                                        act = <temp30>-key ).


    temp33 = sy-tabix.
    READ TABLE lt_langu INDEX 2 ASSIGNING <temp32>.
    sy-tabix = temp33.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `D`
                                        act = <temp32>-key ).


    temp35 = sy-tabix.
    READ TABLE lt_langu INDEX 3 ASSIGNING <temp34>.
    sy-tabix = temp35.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A`
                                        act = <temp34>-key ).


    temp37 = sy-tabix.
    READ TABLE lt_langu INDEX 4 ASSIGNING <temp36>.
    sy-tabix = temp37.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `Q`
                                        act = <temp36>-key ).

  ENDMETHOD.

  METHOD test_languages_no_texts.
    DATA temp38 TYPE z2ui5_cl_popup_dtel_srch=>ty_t_t002t.
    DATA temp9 TYPE string_table.
    DATA lt_langu TYPE z2ui5_cl_popup_dtel_srch=>ty_t_langu.
    FIELD-SYMBOLS <temp39> LIKE LINE OF lt_langu.
    DATA temp40 LIKE sy-tabix.
    FIELD-SYMBOLS <temp41> LIKE LINE OF lt_langu.
    DATA temp42 LIKE sy-tabix.
    DATA temp2 TYPE xsdboolean.
    DATA temp3 LIKE sy-subrc.

    " neither T002T nor T002C readable: the selected languages still have an
    " entry, so selectedKey always matches
    mo_pop->mv_langu  = `F`.
    mo_pop->mv_langu2 = `D`.

    CLEAR temp38.

    CLEAR temp9.

    lt_langu = mo_pop->get_language_list( t002t     = temp38
                                                installed = temp9 ).



    temp40 = sy-tabix.
    READ TABLE lt_langu INDEX 1 ASSIGNING <temp39>.
    sy-tabix = temp40.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `English (E)`
                                        act = <temp39>-text ).


    temp42 = sy-tabix.
    READ TABLE lt_langu INDEX 2 ASSIGNING <temp41>.
    sy-tabix = temp42.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `German (D)`
                                        act = <temp41>-text ).


    READ TABLE lt_langu WITH KEY key = `F` TRANSPORTING NO FIELDS.
    temp3 = sy-subrc.
    temp2 = boolc( temp3 = 0 ).
    cl_abap_unit_assert=>assert_true( temp2 ).

  ENDMETHOD.

  METHOD test_languages_filter.
    DATA temp43 TYPE z2ui5_cl_popup_dtel_srch=>ty_t_t002t.
    DATA temp44 LIKE LINE OF temp43.
    DATA temp10 TYPE string_table.
    DATA lt_langu TYPE z2ui5_cl_popup_dtel_srch=>ty_t_langu.
    FIELD-SYMBOLS <temp45> LIKE LINE OF lt_langu.
    DATA temp46 LIKE sy-tabix.
    FIELD-SYMBOLS <temp47> LIKE LINE OF lt_langu.
    DATA temp48 LIKE sy-tabix.
    FIELD-SYMBOLS <temp49> LIKE LINE OF lt_langu.
    DATA temp50 LIKE sy-tabix.

    " T002C flags D, E and F: exactly those stay - the filter of 9383ad7
    " (DELETE TABLE ... FROM on a table WITH EMPTY KEY) removed the wrong rows
    IF sy-langu <> `E` AND sy-langu <> `D` AND sy-langu <> `F`.
      RETURN.
    ENDIF.

    CLEAR temp43.

    temp44-sprsl = `A`.
    temp44-sptxt = `Albanian`.
    INSERT temp44 INTO TABLE temp43.
    temp44-sprsl = `D`.
    temp44-sptxt = `German`.
    INSERT temp44 INTO TABLE temp43.
    temp44-sprsl = `E`.
    temp44-sptxt = `English`.
    INSERT temp44 INTO TABLE temp43.
    temp44-sprsl = `F`.
    temp44-sptxt = `French`.
    INSERT temp44 INTO TABLE temp43.
    temp44-sprsl = `H`.
    temp44-sptxt = `Hungarian`.
    INSERT temp44 INTO TABLE temp43.
    temp44-sprsl = `Q`.
    temp44-sptxt = `Other`.
    INSERT temp44 INTO TABLE temp43.

    CLEAR temp10.
    INSERT `D` INTO TABLE temp10.
    INSERT `E` INTO TABLE temp10.
    INSERT `F` INTO TABLE temp10.

    lt_langu = mo_pop->get_language_list(
        t002t     = temp43
        installed = temp10 ).

    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lt_langu ) ).


    temp46 = sy-tabix.
    READ TABLE lt_langu INDEX 1 ASSIGNING <temp45>.
    sy-tabix = temp46.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `E`
                                        act = <temp45>-key ).


    temp48 = sy-tabix.
    READ TABLE lt_langu INDEX 2 ASSIGNING <temp47>.
    sy-tabix = temp48.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `D`
                                        act = <temp47>-key ).


    temp50 = sy-tabix.
    READ TABLE lt_langu INDEX 3 ASSIGNING <temp49>.
    sy-tabix = temp50.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `French (F)`
                                        act = <temp49>-text ).

  ENDMETHOD.

ENDCLASS.
