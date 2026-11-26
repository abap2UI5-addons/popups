CLASS z2ui5_cl_popup_shlp DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " abap2ui5lint-disable unbound-public-attribute -- a popup's attributes are its interface to the app that called it, which reads them once the popup returns
    INTERFACES z2ui5_if_app.
    " check_confirmed( ) - a row chosen; value( ) - REF TO string, the value
    " of the search-help parameter i_fname of the row (mv_return_value);
    " value_string( ) - the same value
    INTERFACES z2ui5_if_popup_result.

    " a prefilled selection field: name is the search-help parameter. value
    " is written into its input field of the dialog and selects like a typed
    " value, t_range adds exact conditions (sign, option, low, high)
    TYPES:
      BEGIN OF ty_s_init,
        name    TYPE string,
        value   TYPE string,
        t_range TYPE z2ui5_cl_popup_context=>ty_t_range,
      END OF ty_s_init.
    TYPES ty_t_init TYPE STANDARD TABLE OF ty_s_init WITH DEFAULT KEY.

    DATA mv_searchhelp   TYPE string.
    DATA mv_table        TYPE string.
    DATA mv_fname        TYPE string.
    DATA mv_shlpfield    TYPE string.
    DATA mv_value        TYPE string.
    DATA mv_return_value TYPE string.
    DATA mv_rows         TYPE int1 VALUE '50' ##NEEDED.
    DATA mt_data         TYPE REF TO data.
    DATA ms_data_row     TYPE REF TO data.
    DATA mo_layout       TYPE REF TO z2ui5_cl_layo_manager.
    DATA ms_shlp         TYPE z2ui5_cl_popup_context=>ty_shlp_descr.
    DATA mt_result_desc  TYPE z2ui5_cl_popup_context=>ty_t_dfies_2.
    DATA mr_data         TYPE REF TO data.

    TYPES ty_t_dfies TYPE z2ui5_cl_popup_context=>ty_t_dfies_2 ##NEEDED.

    CLASS-METHODS factory
      IMPORTING
        i_table       TYPE string
        i_fname       TYPE string
        i_value       TYPE string
        i_data        TYPE REF TO data OPTIONAL
        it_init       TYPE ty_t_init   OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_popup_shlp.

    " opens a search help by its name - i_fname is the search-help parameter
    " whose value of the selected row is returned in mv_return_value
    CLASS-METHODS factory_by_searchhelp
      IMPORTING
        i_searchhelp  TYPE string
        i_fname       TYPE string
        it_init       TYPE ty_t_init OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_popup_shlp.
    " abap2ui5lint-enable unbound-public-attribute

  PROTECTED SECTION.
    DATA client  TYPE REF TO z2ui5_if_client.
    DATA mt_init TYPE ty_t_init.
    DATA mv_check_confirmed TYPE abap_bool.

    METHODS on_init.
    METHODS render_view.
    METHODS on_event.
    METHODS on_after_layout.
    METHODS get_layout.
    METHODS set_selopt.

  PRIVATE SECTION.
    METHODS read_search_help.
    METHODS shlp_get_by_name.
    METHODS init_set_row.

    METHODS init_get_selopt
      RETURNING
        VALUE(result) TYPE z2ui5_cl_popup_context=>ty_shlp_descr-selopt.

    METHODS get_title
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.


CLASS z2ui5_cl_popup_shlp IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ) IS NOT INITIAL.
      on_init( ).
      render_view( ).
    ELSE.
      on_event( ).
      on_after_layout( ).
    ENDIF.

  ENDMETHOD.

  METHOD on_init.

    IF mv_searchhelp IS NOT INITIAL.
      shlp_get_by_name( ).
    ENDIF.

    read_search_help( ).

    " the first read builds the row of the selection fields - the prefill
    " goes into it and the search runs again, as if the user had typed it
    IF mt_init IS NOT INITIAL.
      init_set_row( ).
      set_selopt( ).
      read_search_help( ).
    ENDIF.

    get_layout( ).

  ENDMETHOD.

  METHOD get_layout.

    DATA class TYPE string.
    DATA app TYPE string.
    class = z2ui5_cl_popup_context=>rtti_get_classname_by_ref( me ).

    app = z2ui5_cl_popup_context=>url_param_get( val = 'app'
                                              url = client->get( )-s_config-search ).

    mo_layout = z2ui5_cl_layo_manager=>factory( control  = z2ui5_cl_layo_manager=>m_table
                                                data     = mt_data
                                                handle01 = class
                                                handle02 = mv_shlpfield
                                                handle03 = app
                                                handle04 = mv_searchhelp ).

  ENDMETHOD.

  METHOD render_view.

    DATA popup TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA dialog TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA simple_form TYPE REF TO z2ui5_cl_ui5_view_builder.
    FIELD-SYMBOLS <data_row> TYPE data.
    DATA temp10 LIKE LINE OF mt_result_desc.
    DATA dfies LIKE REF TO temp10.
      DATA temp11 TYPE z2ui5_cl_popup_context=>ty_ddshiface-value.
      DATA temp12 TYPE z2ui5_cl_popup_context=>ty_ddshiface.
      DATA enabled TYPE abap_bool.
      DATA temp1 TYPE xsdboolean.
      FIELD-SYMBOLS <val> TYPE any.
    FIELD-SYMBOLS <mt_data> TYPE data.
    DATA table TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA header TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA columns TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA temp13 LIKE LINE OF mo_layout->ms_layout-t_layout.
    DATA layout LIKE REF TO temp13.
      DATA lv_index LIKE sy-tabix.
    DATA cells TYPE REF TO z2ui5_cl_ui5_view_builder.
    popup = z2ui5_cl_ui5_view_builder=>factory(
                      )->ele( n = `FragmentDefinition` ns = `core`
                      )->a( n = `xmlns` v = `sap.m`
                      )->a( n = `xmlns:core` v = `sap.ui.core`
                      )->a( n = `xmlns:form` v = `sap.ui.layout.form` ).


    dialog = popup->ele( `Dialog`
                          )->a( n = `title` v = z2ui5_cl_popup_context=>rtti_get_data_element_texts( `SCRFMTCH`  )-medium
                          )->a( n = `contentWidth` v = `70%`
                          )->a( n = `afterClose` v = client->_event( 'SHLP_CLOSE' ) ).


    simple_form = dialog->ele( n = `SimpleForm` ns = `form`
                                )->a( n = `layout` v = `ResponsiveGridLayout`
                                )->a( n = `editable` b = abap_true
                                )->ele( n = `content` ns = `form` ).


    ASSIGN ms_data_row->* TO <data_row>.

    " loop over all components


    LOOP AT mt_result_desc REFERENCE INTO dfies.

      " fixed values of the search help are not editable

      CLEAR temp11.

      READ TABLE ms_shlp-interface INTO temp12 WITH KEY shlpfield = dfies->fieldname.
      IF sy-subrc = 0.
        temp11 = temp12-value.
      ENDIF.


      temp1 = boolc( temp11 IS INITIAL ).
      enabled = temp1.

      " the row carries the list fields of the search help only, the record
      " description every field - without the check the input of a field
      " that is not listed was bound to the previous field, or the first
      " one to an unassigned field symbol

      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <data_row> TO <val>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      simple_form->tag( `Label`
          )->a( n = `text` v = z2ui5_cl_popup_context=>rtti_get_data_element_text_l( dfies->rollname ) ).

      simple_form->tag( `Input`
          )->a( n = `value` v = client->_bind( <val> )
          )->a( n = `showValueHelp` b = abap_false
          )->a( n = `submit` v = client->_event( 'SHLP_INPUT_DONE' )
          )->a( n = `enabled` b = enabled ).

    ENDLOOP.


    ASSIGN mt_data->* TO <mt_data>.


    table = dialog->ele( `Table`
                      )->a( n = `growing`    v = `true`
                      )->a( n = `width`      v = `auto`
                      )->a( n = `items`      v = client->_bind( <mt_data> )
                      )->a( n = `headerText` v = get_title( ) ).


    header = table->ele( `headerToolbar`
                       )->ele( `OverflowToolbar`
                       )->tag( `Title`
                       )->a( n = `text` v = get_title( )
                       )->tag( `ToolbarSpacer` ).

    header = z2ui5_cl_layo_pop=>render_layout_function( xml    = header
                                                        client = client
                                                        layout = mo_layout ).


    columns = table->ele( `columns` ).



    LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO layout.

      lv_index = sy-tabix.

      columns->ele( `Column`
          )->a( n = `visible` v = client->_bind( val       = layout->visible
                                                        tab       = mo_layout->ms_layout-t_layout
                                                        tab_index = lv_index )
          )->a( n = `mergeDuplicates` v = client->_bind( val       = layout->merge
                                                        tab       = mo_layout->ms_layout-t_layout
                                                        tab_index = lv_index )
          )->a( n = `minScreenWidth` v = client->_bind( val       = layout->width
                                                        tab       = mo_layout->ms_layout-t_layout
                                                        tab_index = lv_index )
          )->tag( `Text`
          )->a( n = `text` v = z2ui5_cl_popup_context=>rtti_get_data_element_text_l( layout->rollname ) ).

    ENDLOOP.


    cells = columns->end(
                      )->ele( `items`
                      )->ele( `ColumnListItem`
                      )->a( n = `vAlign` v = `Middle`
                      )->a( n = `type` v = `Navigation`
                      )->a( n = `press` v = client->_event( val   = 'SHLP_ROW_SELECT'
                                                                    arg   = `${ROW_ID}` )
                      )->ele( `cells` ).

    LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO layout.

      cells->ele( `ObjectIdentifier`
          )->a( n = `text` v = |\{{ z2ui5_cl_ui5_view_builder=>component_path( layout->fname ) }\}| ).

    ENDLOOP.

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD on_event.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
        DATA lt_arg TYPE string_table.
        FIELD-SYMBOLS <row> TYPE any.
        FIELD-SYMBOLS <temp1> LIKE LINE OF lt_arg.
        DATA temp2 LIKE sy-tabix.
        FIELD-SYMBOLS <value> TYPE any.

    CASE client->get( )-event.

      WHEN `SHLP_CLOSE`.

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN `SHLP_ROW_SELECT`.


        lt_arg = client->get( )-t_event_arg.
        " an event without the row index raised CX_SY_ITAB_LINE_NOT_FOUND
        IF lt_arg IS INITIAL.
          RETURN.
        ENDIF.

        ASSIGN mt_data->* TO <tab>.




        temp2 = sy-tabix.
        READ TABLE lt_arg INDEX 1 ASSIGNING <temp1>.
        sy-tabix = temp2.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        READ TABLE <tab> INDEX <temp1> ASSIGNING <row>.
        " the index comes from the client - a row that is not there must not
        " reach ASSIGN COMPONENT on an unassigned field symbol
        IF <row> IS NOT ASSIGNED.
          RETURN.
        ENDIF.


        ASSIGN COMPONENT mv_shlpfield OF STRUCTURE <row> TO <value>.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        mv_return_value = <value>.
        mv_check_confirmed = abap_true.

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'SHLP_INPUT_DONE'.

        set_selopt( ).
        read_search_help( ).

      WHEN OTHERS.

        z2ui5_cl_layo_pop=>on_event_layout( client = client
                                            layout = mo_layout ).

    ENDCASE.

  ENDMETHOD.

  METHOD factory.
      DATA t_comp TYPE abap_component_tab.
      DATA struct_desc TYPE REF TO cl_abap_structdescr.
      FIELD-SYMBOLS <i_data> TYPE data.
      FIELD-SYMBOLS <mr_data> TYPE data.

    CREATE OBJECT result.

    result->mv_table = i_table.
    result->mv_fname = i_fname.
    result->mv_value = i_value.
    result->mt_init  = it_init.

    IF i_data IS SUPPLIED.


      t_comp = z2ui5_cl_popup_context=>rtti_get_t_attri_by_any( i_data ).

      struct_desc = cl_abap_structdescr=>create( t_comp ).
      CREATE DATA result->mr_data TYPE HANDLE struct_desc.


      ASSIGN i_data->* TO <i_data>.

      ASSIGN result->mr_data->* TO <mr_data>.

      <mr_data> = <i_data>.

    ENDIF.

  ENDMETHOD.

  METHOD factory_by_searchhelp.

    CREATE OBJECT result.

    result->mv_searchhelp = to_upper( i_searchhelp ).
    result->mv_fname      = to_upper( i_fname ).
    result->mv_shlpfield  = result->mv_fname.
    result->mt_init       = it_init.

  ENDMETHOD.

  METHOD on_after_layout.
          DATA temp14 TYPE REF TO z2ui5_cl_layo_pop.
          DATA app LIKE temp14.

    " only relevant when returning from another app
    IF client->check_on_navigated( ) IS NOT INITIAL.
      TRY.

          temp14 ?= client->get_app( client->get( )-s_draft-id_prev_app ).

          app = temp14.
          mo_layout = app->mo_layout.
          render_view( ).

        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD set_selopt.
    FIELD-SYMBOLS <data_row> TYPE data.
    DATA dfies LIKE LINE OF mt_result_desc.
      FIELD-SYMBOLS <value> TYPE any.
      DATA temp15 TYPE z2ui5_cl_popup_context=>ty_shlp_descr-selopt.
      DATA temp16 LIKE LINE OF temp15.

    CLEAR ms_shlp-selopt.


    ASSIGN ms_data_row->* TO <data_row>.


    LOOP AT mt_result_desc INTO dfies.


      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <data_row> TO <value>.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      IF <value> IS INITIAL.
        CONTINUE.
      ENDIF.


      CLEAR temp15.
      temp15 = ms_shlp-selopt.

      temp16-shlpfield = dfies-fieldname.
      temp16-shlpname = ''.
      temp16-sign = 'I'.
      temp16-option = 'CP'.
      temp16-low = |*{ <value> }*|.
      INSERT temp16 INTO TABLE temp15.
      ms_shlp-selopt = temp15.

    ENDLOOP.

    " the prefill that has no input field in the dialog stays selected
    INSERT LINES OF init_get_selopt( ) INTO TABLE ms_shlp-selopt.

  ENDMETHOD.

  METHOD read_search_help.

    z2ui5_cl_popup_context=>bus_search_help_read( CHANGING ms_shlp        = ms_shlp
                                                      mv_fname       = mv_fname
                                                      mv_table       = mv_table
                                                      mr_data        = mr_data
                                                      mt_result_desc = mt_result_desc
                                                      mv_shlpfield   = mv_shlpfield
                                                      mt_data        = mt_data
                                                      ms_data_row    = ms_data_row ).

  ENDMETHOD.

  METHOD shlp_get_by_name.

    " the description is filled here, so that bus_search_help_read takes it
    " as it is instead of determining one by table and field
    DATA lr_shlp     TYPE REF TO data.
    DATA lr_t_shlp   TYPE REF TO data.
    DATA lv_shlpname TYPE c LENGTH 30.

    FIELD-SYMBOLS <shlp>   TYPE any.
    FIELD-SYMBOLS <t_shlp> TYPE STANDARD TABLE.
    DATA lv_type TYPE string.
    DATA lv_fm TYPE string.
    DATA lv_type_t TYPE string.
    FIELD-SYMBOLS <elementary> TYPE any.
    DATA temp17 TYPE z2ui5_cl_popup_context=>ty_ddshiface-shlpfield.
    DATA temp18 TYPE z2ui5_cl_popup_context=>ty_ddshiface.
    DATA lv_shlpfield LIKE temp17.

    lv_shlpname = mv_searchhelp.


    lv_type = `SHLP_DESCR`.
    CREATE DATA lr_shlp TYPE (lv_type).
    ASSIGN lr_shlp->* TO <shlp>.


    lv_fm = `F4IF_GET_SHLP_DESCR`.
    CALL FUNCTION lv_fm
      EXPORTING
        shlpname = lv_shlpname
      IMPORTING
        shlp     = <shlp>.

    MOVE-CORRESPONDING <shlp> TO ms_shlp.

    IF ms_shlp-fieldprop IS INITIAL.
      RAISE EXCEPTION TYPE z2ui5_cx_popup_error
        EXPORTING
          val = |Search help { lv_shlpname } not found|.
    ENDIF.

    IF ms_shlp-intdescr-issimple = abap_true.
      RETURN.
    ENDIF.

    " a collective search help - its first elementary search help is used,
    " the returned parameter follows the mapping of the collective one

    lv_type_t = `SHLP_DESCT`.
    CREATE DATA lr_t_shlp TYPE (lv_type_t).
    ASSIGN lr_t_shlp->* TO <t_shlp>.

    lv_fm = `F4IF_EXPAND_SEARCHHELP`.
    CALL FUNCTION lv_fm
      EXPORTING
        shlp_top = <shlp>
      IMPORTING
        shlp_tab = <t_shlp>.


    READ TABLE <t_shlp> INDEX 1 ASSIGNING <elementary>.
    IF <elementary> IS NOT ASSIGNED.
      RAISE EXCEPTION TYPE z2ui5_cx_popup_error
        EXPORTING
          val = |Search help { lv_shlpname } has no elementary search help|.
    ENDIF.

    MOVE-CORRESPONDING <elementary> TO ms_shlp.


    CLEAR temp17.

    READ TABLE ms_shlp-interface INTO temp18 WITH KEY topshlpfld = mv_fname.
    IF sy-subrc = 0.
      temp17 = temp18-shlpfield.
    ENDIF.

    lv_shlpfield = temp17.
    IF lv_shlpfield IS NOT INITIAL.
      mv_shlpfield = lv_shlpfield.
    ENDIF.

  ENDMETHOD.

  METHOD init_set_row.

    FIELD-SYMBOLS <data_row> TYPE data.
    DATA init LIKE LINE OF mt_init.
      DATA temp19 TYPE string.
FIELD-SYMBOLS <value> TYPE any.
    ASSIGN ms_data_row->* TO <data_row>.


    LOOP AT mt_init INTO init WHERE value IS NOT INITIAL.


      temp19 = to_upper( init-name ).

ASSIGN COMPONENT temp19 OF STRUCTURE <data_row> TO <value>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      <value> = init-value.

    ENDLOOP.

  ENDMETHOD.

  METHOD init_get_selopt.

    FIELD-SYMBOLS <value> TYPE any.

    FIELD-SYMBOLS <data_row> TYPE data.
    DATA init LIKE LINE OF mt_init.
      DATA lv_name TYPE string.
      DATA range LIKE LINE OF init-t_range.
        DATA temp20 TYPE z2ui5_cl_popup_context=>ty_shlp_descr-selopt.
        DATA temp21 LIKE LINE OF temp20.
      DATA temp22 TYPE z2ui5_cl_popup_context=>ty_shlp_descr-selopt.
      DATA temp23 LIKE LINE OF temp22.
      DATA temp3 TYPE z2ui5_cl_popup_context=>ty_ddshselopt-option.
    ASSIGN ms_data_row->* TO <data_row>.


    LOOP AT mt_init INTO init.


      lv_name = to_upper( init-name ).


      LOOP AT init-t_range INTO range.

        CLEAR temp20.
        temp20 = result.

        temp21-shlpfield = lv_name.
        temp21-sign = range-sign.
        temp21-option = range-option.
        temp21-low = range-low.
        temp21-high = range-high.
        INSERT temp21 INTO TABLE temp20.
        result = temp20.
      ENDLOOP.

      IF init-value IS INITIAL.
        CONTINUE.
      ENDIF.

      " a value with an input field is selected by set_selopt like typed input
      UNASSIGN <value>.
      ASSIGN COMPONENT lv_name OF STRUCTURE <data_row> TO <value>.
      IF <value> IS ASSIGNED.
        CONTINUE.
      ENDIF.


      CLEAR temp22.
      temp22 = result.

      temp23-shlpfield = lv_name.
      temp23-sign = `I`.

      IF init-value CA `*+`.
        temp3 = `CP`.
      ELSE.
        temp3 = `EQ`.
      ENDIF.
      temp23-option = temp3.
      temp23-low = init-value.
      INSERT temp23 INTO TABLE temp22.
      result = temp22.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_title.
    DATA temp24 TYPE string.

    IF mv_table IS NOT INITIAL.
      result = z2ui5_cl_popup_context=>rtti_get_table_desrc( mv_table ).
      RETURN.
    ENDIF.


    IF ms_shlp-intdescr-ddtext IS NOT INITIAL.
      temp24 = ms_shlp-intdescr-ddtext.
    ELSE.
      temp24 = mv_searchhelp.
    ENDIF.
    result = temp24.

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~check_confirmed.

    result = mv_check_confirmed.

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~value.

    GET REFERENCE OF mv_return_value INTO result.

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~value_string.

    result = mv_return_value.

  ENDMETHOD.

ENDCLASS.
