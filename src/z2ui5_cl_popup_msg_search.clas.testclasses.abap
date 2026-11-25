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
    mo_action = NEW #( NEW z2ui5_cl_ui5_handler( `` ) ).
    mo_action->mo_app->mo_app = io_app.
    mo_action->mo_app->mv_check_initialized = abap_true.
    mo_action->ms_actual-event = iv_event.
    mo_action->ms_actual-t_event_arg = it_arg.
    io_app->main( NEW z2ui5_cl_ui5_client( mo_action ) ).

  ENDMETHOD.

  METHOD test_initial_not_confirmed.

    DATA(lo_pop) = z2ui5_cl_popup_msg_search=>factory( ).
    DATA(li_result) = CAST z2ui5_if_popup_result( lo_pop ).

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

  METHOD test_confirmed.

    DATA(lo_pop) = z2ui5_cl_popup_msg_search=>factory( ).
    lo_pop->mt_result = VALUE #( ( selkz = abap_true arbgb = `00` msgnr = `001` text = `&1&2&3&4` ) ).
    roundtrip( io_app   = lo_pop
               iv_event = `CONFIRM` ).
    DATA(li_result) = CAST z2ui5_if_popup_result( lo_pop ).

    cl_abap_unit_assert=>assert_true( li_result->check_confirmed( ) ).
    DATA(lr_result) = CAST z2ui5_cl_popup_msg_search=>ty_s_result( li_result->value( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `00`
                                        act = lr_result->msgid ).
    cl_abap_unit_assert=>assert_equals( exp = `001`
                                        act = lr_result->msgno ).
    cl_abap_unit_assert=>assert_initial( li_result->value_string( ) ).

  ENDMETHOD.

  METHOD test_cancelled.

    DATA(lo_pop) = z2ui5_cl_popup_msg_search=>factory( ).
    roundtrip( io_app   = lo_pop
               iv_event = `CANCEL` ).
    DATA(li_result) = CAST z2ui5_if_popup_result( lo_pop ).

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

    mo_action = NEW #( NEW z2ui5_cl_ui5_handler( `` ) ).
    mo_pop = z2ui5_cl_popup_msg_search=>factory( ).
    mo_action->mo_app->mo_app = mo_pop.
    mo_action->mo_app->mv_check_initialized = abap_true.
    mo_pop->client = NEW z2ui5_cl_ui5_client( mo_action ).

  ENDMETHOD.

  METHOD front_xml.

    LOOP AT mo_action->ms_next-t_action_front INTO DATA(ls_action)
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

    DATA(lo_page) = z2ui5_cl_ui5_view_builder=>factory( )->ele( `Page` ).
    mo_pop->z2ui5_if_popup_embed~mo_parent_view = lo_page.

    CAST z2ui5_if_app( mo_pop )->main( mo_pop->client ).

    " no popup, no navigation - the host displays its page
    cl_abap_unit_assert=>assert_initial( front_xml( `display` ) ).
    cl_abap_unit_assert=>assert_not_bound( mo_action->ms_next-o_app_leave ).
    cl_abap_unit_assert=>assert_true( mo_pop->z2ui5_if_popup_embed~mv_view_display ).

    DATA(lv_xml) = lo_page->stringify( ).
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

    mo_pop->z2ui5_if_popup_embed~mo_parent_view = z2ui5_cl_ui5_view_builder=>factory( )->ele( `Page` ).
    CAST z2ui5_if_app( mo_pop )->main( mo_pop->client ).
    CLEAR mo_pop->z2ui5_if_popup_embed~mv_view_display.

    " a later roundtrip with an event of the host, then CANCEL and CONFIRM:
    " embedded, the app never leaves and never asks for a new display
    LOOP AT VALUE string_table( ( `HOST_EVENT` ) ( `CANCEL` ) ( `CONFIRM` ) ) INTO DATA(lv_event).
      init_popup( ).
      mo_pop->z2ui5_if_popup_embed~mo_parent_view = z2ui5_cl_ui5_view_builder=>factory( )->ele( `Page` ).
      mo_pop->embed_initialized = abap_true.
      mo_action->ms_actual-event = lv_event.
      CAST z2ui5_if_app( mo_pop )->main( mo_pop->client ).
      cl_abap_unit_assert=>assert_not_bound( mo_action->ms_next-o_app_leave ).
      cl_abap_unit_assert=>assert_false( mo_pop->z2ui5_if_popup_embed~mv_view_display ).
      cl_abap_unit_assert=>assert_false( CAST z2ui5_if_popup_result( mo_pop )->check_confirmed( ) ).
      cl_abap_unit_assert=>assert_initial( front_xml( `display` ) ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_languages_installed.

    " T002C flags other languages only (seen on a system): English and
    " German are offered all the same, and first
    DATA(lt_langu) = mo_pop->get_language_list(
        t002t     = VALUE #( ( sprsl = `A` sptxt = `Albanian` )
                             ( sprsl = `D` sptxt = `German` )
                             ( sprsl = `E` sptxt = `English` )
                             ( sprsl = `P` sptxt = `Portuguese` )
                             ( sprsl = `Q` sptxt = `Other` ) )
        installed = VALUE #( ( `P` ) ( `A` ) ) ).

    cl_abap_unit_assert=>assert_equals( exp = `E`
                                        act = lt_langu[ 1 ]-key ).
    cl_abap_unit_assert=>assert_equals( exp = `English (E)`
                                        act = lt_langu[ 1 ]-text ).
    cl_abap_unit_assert=>assert_equals( exp = `D`
                                        act = lt_langu[ 2 ]-key ).
    cl_abap_unit_assert=>assert_equals( exp = `A`
                                        act = lt_langu[ 3 ]-key ).
    cl_abap_unit_assert=>assert_equals( exp = `P`
                                        act = lt_langu[ 4 ]-key ).
    IF sy-langu <> `Q`.
      cl_abap_unit_assert=>assert_false( xsdbool( line_exists( lt_langu[ key = `Q` ] ) ) ).
    ENDIF.

  ENDMETHOD.

  METHOD test_languages_no_filter.

    " T002C not readable: every language of T002T, English and German first
    DATA(lt_langu) = mo_pop->get_language_list(
        t002t     = VALUE #( ( sprsl = `A` sptxt = `Albanian` )
                             ( sprsl = `D` sptxt = `German` )
                             ( sprsl = `E` sptxt = `English` )
                             ( sprsl = `Q` sptxt = `Other` ) )
        installed = VALUE #( ) ).

    cl_abap_unit_assert=>assert_equals( exp = `E`
                                        act = lt_langu[ 1 ]-key ).
    cl_abap_unit_assert=>assert_equals( exp = `D`
                                        act = lt_langu[ 2 ]-key ).
    cl_abap_unit_assert=>assert_equals( exp = `A`
                                        act = lt_langu[ 3 ]-key ).
    cl_abap_unit_assert=>assert_equals( exp = `Q`
                                        act = lt_langu[ 4 ]-key ).

  ENDMETHOD.

  METHOD test_languages_no_texts.

    " neither T002T nor T002C readable: the selected languages still have an
    " entry, so selectedKey always matches
    mo_pop->mv_langu  = `F`.
    mo_pop->mv_langu2 = `D`.
    DATA(lt_langu) = mo_pop->get_language_list( t002t     = VALUE #( )
                                                installed = VALUE #( ) ).

    cl_abap_unit_assert=>assert_equals( exp = `English (E)`
                                        act = lt_langu[ 1 ]-text ).
    cl_abap_unit_assert=>assert_equals( exp = `German (D)`
                                        act = lt_langu[ 2 ]-text ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_langu[ key = `F` ] ) ) ).

  ENDMETHOD.

  METHOD test_languages_filter.

    " T002C flags D, E and F: exactly those stay - the filter of 9383ad7
    " (DELETE TABLE ... FROM on a table WITH EMPTY KEY) removed the wrong rows
    IF sy-langu <> `E` AND sy-langu <> `D` AND sy-langu <> `F`.
      RETURN.
    ENDIF.
    DATA(lt_langu) = mo_pop->get_language_list(
        t002t     = VALUE #( ( sprsl = `A` sptxt = `Albanian` )
                             ( sprsl = `D` sptxt = `German` )
                             ( sprsl = `E` sptxt = `English` )
                             ( sprsl = `F` sptxt = `French` )
                             ( sprsl = `H` sptxt = `Hungarian` )
                             ( sprsl = `Q` sptxt = `Other` ) )
        installed = VALUE #( ( `D` ) ( `E` ) ( `F` ) ) ).

    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lt_langu ) ).
    cl_abap_unit_assert=>assert_equals( exp = `E`
                                        act = lt_langu[ 1 ]-key ).
    cl_abap_unit_assert=>assert_equals( exp = `D`
                                        act = lt_langu[ 2 ]-key ).
    cl_abap_unit_assert=>assert_equals( exp = `French (F)`
                                        act = lt_langu[ 3 ]-text ).

  ENDMETHOD.

ENDCLASS.
