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
    " deprecated: the value of factory( i_value ) - the popup does not use
    " it; a prefill goes into it_init (name and value)
    DATA mv_value        TYPE string.
    DATA mv_return_value TYPE string.
    " the most rows F4IF_SELECT_VALUES reads (0 reads all)
    DATA mv_rows         TYPE i VALUE 50.
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
    DATA temp10 TYPE string.
    DATA searchhelp LIKE temp10.
    class = z2ui5_cl_popup_context=>rtti_get_classname_by_ref( me ).

    app = z2ui5_cl_popup_context=>url_param_get( val = 'app'
                                              url = client->get( )-s_config-search ).

    " opened by table and field, mv_searchhelp is empty - the layout is the
    " one of the search help determined for the field, not one shared by
    " every search help with a parameter of that name

    IF mv_searchhelp IS NOT INITIAL.
      temp10 = mv_searchhelp.
    ELSE.
      temp10 = ms_shlp-shlpname.
    ENDIF.

    searchhelp = temp10.

    mo_layout = z2ui5_cl_layo_manager=>factory( control  = z2ui5_cl_layo_manager=>m_table
                                                data     = mt_data
                                                handle01 = class
                                                handle02 = mv_shlpfield
                                                handle03 = app
                                                handle04 = searchhelp ).

  ENDMETHOD.

  METHOD render_view.

    DATA popup TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA dialog TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA simple_form TYPE REF TO z2ui5_cl_ui5_view_builder.
    FIELD-SYMBOLS <data_row> TYPE data.
    DATA temp11 LIKE LINE OF mt_result_desc.
    DATA dfies LIKE REF TO temp11.
      DATA temp12 TYPE z2ui5_cl_popup_context=>ty_ddshiface-value.
      DATA temp13 TYPE z2ui5_cl_popup_context=>ty_ddshiface.
      DATA enabled TYPE abap_bool.
      DATA temp1 TYPE xsdboolean.
      FIELD-SYMBOLS <val> TYPE any.
    FIELD-SYMBOLS <mt_data> TYPE data.
    DATA table TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA header TYPE REF TO z2ui5_cl_ui5_view_builder.
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

      CLEAR temp12.

      READ TABLE ms_shlp-interface INTO temp13 WITH KEY shlpfield = dfies->fieldname.
      IF sy-subrc = 0.
        temp12 = temp13-value.
      ENDIF.


      temp1 = boolc( temp12 IS INITIAL ).
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

    z2ui5_cl_popup_value_help=>render_columns( table             = table
                                               client            = client
                                               layout            = mo_layout
                                               event             = `SHLP_ROW_SELECT`
                                               label_by_rollname = abap_true ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD on_event.

    DATA lv_value TYPE string.
    DATA lv_found TYPE abap_bool.

    CASE client->get( )-event.

      WHEN `SHLP_CLOSE`.

        client->popup_destroy( ).

        client->nav_app_leave( ).

      WHEN `SHLP_ROW_SELECT`.

        z2ui5_cl_popup_value_help=>read_row_value( EXPORTING data  = mt_data
                                                             t_arg = client->get( )-t_event_arg
                                                             fname = mv_shlpfield
                                                   IMPORTING value = lv_value
                                                             found = lv_found ).
        IF lv_found = abap_false.
          RETURN.
        ENDIF.

        mv_return_value = lv_value.
        mv_check_confirmed = abap_true.

        client->popup_destroy( ).

        client->nav_app_leave( ).

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
    DATA layout TYPE REF TO z2ui5_cl_layo_manager.

    " only relevant when returning from another app
    IF client->check_on_navigated( ) = abap_false.
      RETURN.
    ENDIF.


    layout = z2ui5_cl_popup_value_help=>get_layout_back( client ).
    IF layout IS BOUND.
      mo_layout = layout.
    ENDIF.
    " back from the layout popup, or a restored draft (browser Forward, a
    " reload) whose popup went with the page - show it (again)
    render_view( ).

  ENDMETHOD.

  METHOD set_selopt.
    FIELD-SYMBOLS <data_row> TYPE data.
    DATA dfies LIKE LINE OF mt_result_desc.
      FIELD-SYMBOLS <value> TYPE any.
      DATA temp14 TYPE z2ui5_cl_popup_context=>ty_shlp_descr-selopt.
      DATA temp15 LIKE LINE OF temp14.

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


      CLEAR temp14.
      temp14 = ms_shlp-selopt.

      temp15-shlpfield = dfies-fieldname.
      temp15-shlpname = ''.
      temp15-sign = 'I'.
      temp15-option = 'CP'.
      temp15-low = |*{ <value> }*|.
      INSERT temp15 INTO TABLE temp14.
      ms_shlp-selopt = temp14.

    ENDLOOP.

    " the prefill that has no input field in the dialog stays selected
    INSERT LINES OF init_get_selopt( ) INTO TABLE ms_shlp-selopt.

  ENDMETHOD.

  METHOD read_search_help.

    z2ui5_cl_popup_context=>bus_search_help_read( EXPORTING iv_max_rows    = mv_rows
                                                  CHANGING  ms_shlp        = ms_shlp
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
    DATA temp16 TYPE z2ui5_cl_popup_context=>ty_ddshiface-shlpfield.
    DATA temp17 TYPE z2ui5_cl_popup_context=>ty_ddshiface.
    DATA lv_shlpfield LIKE temp16.

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


    CLEAR temp16.

    READ TABLE ms_shlp-interface INTO temp17 WITH KEY topshlpfld = mv_fname.
    IF sy-subrc = 0.
      temp16 = temp17-shlpfield.
    ENDIF.

    lv_shlpfield = temp16.
    IF lv_shlpfield IS NOT INITIAL.
      mv_shlpfield = lv_shlpfield.
    ENDIF.

  ENDMETHOD.

  METHOD init_set_row.

    FIELD-SYMBOLS <data_row> TYPE data.
    DATA init LIKE LINE OF mt_init.
      DATA temp18 TYPE string.
FIELD-SYMBOLS <value> TYPE any.
    ASSIGN ms_data_row->* TO <data_row>.


    LOOP AT mt_init INTO init WHERE value IS NOT INITIAL.


      temp18 = to_upper( init-name ).

ASSIGN COMPONENT temp18 OF STRUCTURE <data_row> TO <value>.
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
        DATA temp19 TYPE z2ui5_cl_popup_context=>ty_shlp_descr-selopt.
        DATA temp20 LIKE LINE OF temp19.
      DATA temp21 TYPE z2ui5_cl_popup_context=>ty_shlp_descr-selopt.
      DATA temp22 LIKE LINE OF temp21.
      DATA temp3 TYPE z2ui5_cl_popup_context=>ty_ddshselopt-option.
    ASSIGN ms_data_row->* TO <data_row>.


    LOOP AT mt_init INTO init.


      lv_name = to_upper( init-name ).


      LOOP AT init-t_range INTO range.

        CLEAR temp19.
        temp19 = result.

        temp20-shlpfield = lv_name.
        temp20-sign = range-sign.
        temp20-option = range-option.
        temp20-low = range-low.
        temp20-high = range-high.
        INSERT temp20 INTO TABLE temp19.
        result = temp19.
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


      CLEAR temp21.
      temp21 = result.

      temp22-shlpfield = lv_name.
      temp22-sign = `I`.

      IF init-value CA `*+`.
        temp3 = `CP`.
      ELSE.
        temp3 = `EQ`.
      ENDIF.
      temp22-option = temp3.
      temp22-low = init-value.
      INSERT temp22 INTO TABLE temp21.
      result = temp21.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_title.
    DATA temp23 TYPE string.

    IF mv_table IS NOT INITIAL.
      result = z2ui5_cl_popup_context=>rtti_get_table_desrc( mv_table ).
      RETURN.
    ENDIF.


    IF ms_shlp-intdescr-ddtext IS NOT INITIAL.
      temp23 = ms_shlp-intdescr-ddtext.
    ELSE.
      temp23 = mv_searchhelp.
    ENDIF.
    result = temp23.

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
