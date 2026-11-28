CLASS z2ui5_cl_popup_value_help DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC
  " z2ui5_cl_popup_shlp shows a search help the same way - it takes the
  " columns, the pressed row and the return from the layout popup from the
  " static helpers below
  GLOBAL FRIENDS z2ui5_cl_popup_shlp.

  PUBLIC SECTION.
    " abap2ui5lint-disable unbound-public-attribute -- a popup's attributes are its interface to the app that called it, which reads them once the popup returns
    INTERFACES z2ui5_if_app.
    " check_confirmed( ) - a row chosen; value( ) - REF TO string, the value
    " of the field i_fname of the row (mv_return_value); value_string( ) -
    " the same value
    INTERFACES z2ui5_if_popup_result.

    DATA mt_data         TYPE REF TO data.
    DATA ms_data_row     TYPE REF TO data.
    DATA mo_layout       TYPE REF TO z2ui5_cl_layo_manager.

    DATA mv_table        TYPE string.
    DATA mv_field        TYPE string.
    DATA mv_value        TYPE string.
    DATA mv_return_value TYPE string.
    " the number of rows read - typed in the dialog, cut to 1 to
    " cv_max_rows when read (an INT1 overflowed above 255, 0 read all rows)
    DATA mv_rows         TYPE i VALUE 50.
    DATA mt_dfies        TYPE z2ui5_cl_popup_context=>ty_t_dfies.

    CLASS-METHODS factory
      IMPORTING
        i_table       TYPE string
        i_fname       TYPE string
        i_value       TYPE string
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_popup_value_help.
    " abap2ui5lint-enable unbound-public-attribute

  PROTECTED SECTION.
    CONSTANTS cv_max_rows TYPE i VALUE 999.

    DATA client             TYPE REF TO z2ui5_if_client.
    DATA mv_check_tab_field TYPE string.
    DATA mv_check_tab       TYPE string.
    DATA mv_check_confirmed TYPE abap_bool.

    METHODS get_dfies.

    METHODS on_init.

    METHODS render_view.

    METHODS on_event.

    METHODS set_row_id.

    METHODS get_data
      IMPORTING
        !where TYPE string.

    METHODS prefill_inputs.

    METHODS on_after_layout.

    METHODS get_layout.

    METHODS create_objects.

  PRIVATE SECTION.
    "! the columns and the row template of the table of a value or search
    "! help - one column per position of the layout, the row pressed raises
    "! event with the row id
    "! @parameter label_by_rollname | the column header is the long text of
    "!                    the data element, not the label of the layout
    CLASS-METHODS render_columns
      IMPORTING
        !table            TYPE REF TO z2ui5_cl_ui5_view_builder
        client            TYPE REF TO z2ui5_if_client
        layout            TYPE REF TO z2ui5_cl_layo_manager
        !event            TYPE string
        label_by_rollname TYPE abap_bool DEFAULT abap_false.

    "! the value of component fname of the row the user pressed; its index is
    "! the first event argument, which comes from the client
    CLASS-METHODS read_row_value
      IMPORTING
        !data  TYPE REF TO data
        t_arg  TYPE string_table
        fname  TYPE clike
      EXPORTING
        !value TYPE string
        found  TYPE abap_bool.

    "! the layout of the layout popup the app returns from - unbound for any
    "! other roundtrip
    CLASS-METHODS get_layout_back
      IMPORTING
        client        TYPE REF TO z2ui5_if_client
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_layo_manager.

ENDCLASS.


CLASS z2ui5_cl_popup_value_help IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ) IS NOT INITIAL.
      on_init( ).

      IF mv_check_tab IS INITIAL.
        RETURN.
      ENDIF.
      render_view( ).
      " the first roundtrip has no event - it reached the layout events
      RETURN.

    ENDIF.

    on_event( ).
    on_after_layout( ).

  ENDMETHOD.

  METHOD on_init.
    DATA result TYPE string.

    get_dfies( ).

    IF mv_check_tab IS INITIAL.
      RETURN.
    ENDIF.

    create_objects( ).
    prefill_inputs( ).


    result = z2ui5_cl_popup_context=>tab_get_where_by_dfies( mv_check_tab_field = mv_check_tab_field
                                                              ms_data_row        = ms_data_row
                                                              it_dfies           = mt_dfies ).

    get_data( result ).
    get_layout( ).

  ENDMETHOD.

  METHOD create_objects.

    DATA index TYPE int4.
        DATA temp10 TYPE cl_abap_structdescr=>component_table.
        DATA temp11 LIKE LINE OF temp10.
        DATA temp3 TYPE REF TO cl_abap_datadescr.
        DATA comp LIKE temp10.
        DATA new_struct_desc TYPE REF TO cl_abap_structdescr.
        DATA new_table_desc TYPE REF TO cl_abap_tabledescr.

    TRY.


        CLEAR temp10.

        temp11-name = 'ROW_ID'.

        temp3 ?= cl_abap_datadescr=>describe_by_data( index ).
        temp11-type = temp3.
        INSERT temp11 INTO TABLE temp10.

        comp = temp10.

        INSERT LINES OF z2ui5_cl_popup_context=>rtti_get_t_attri_by_table_name( mv_check_tab  ) INTO TABLE comp.


        new_struct_desc = cl_abap_structdescr=>create( comp ).


        new_table_desc = cl_abap_tabledescr=>create( p_line_type  = new_struct_desc
                                                           p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA mt_data     TYPE HANDLE new_table_desc.
        CREATE DATA ms_data_row TYPE HANDLE new_struct_desc.

      CATCH cx_root ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.

  METHOD get_data.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    IF mv_rows < 1.
      mv_rows = 1.
    ELSEIF mv_rows > cv_max_rows.
      mv_rows = cv_max_rows.
    ENDIF.

    TRY.
        ASSIGN mt_data->* TO <table>.

        SELECT *
          FROM (mv_check_tab) UP TO mv_rows ROWS INTO CORRESPONDING FIELDS OF TABLE <table>
          WHERE (where)

          .

        IF sy-subrc <> 0.
          client->message_toast_display( 'No entries found.' ).
        ENDIF.

        set_row_id( ).

      CATCH cx_root.
        client->message_toast_display( 'Table not released.' ).
    ENDTRY.

  ENDMETHOD.

  METHOD render_view.

    DATA popup TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA dialog TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA simple_form TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA temp12 LIKE LINE OF mt_dfies.
    DATA dfies LIKE REF TO temp12.
      FIELD-SYMBOLS <row> TYPE data.
      FIELD-SYMBOLS <val> TYPE any.
    FIELD-SYMBOLS <table> TYPE data.
    DATA table TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA header TYPE REF TO z2ui5_cl_ui5_view_builder.
    popup = z2ui5_cl_ui5_view_builder=>factory(
                      )->ele( n = `FragmentDefinition` ns = `core`
                      )->a( n = `xmlns` v = `sap.m`
                      )->a( n = `xmlns:core` v = `sap.ui.core`
                      )->a( n = `xmlns:form` v = `sap.ui.layout.form` ).


    dialog = popup->ele( `Dialog`
                          )->a( n = `title` v = z2ui5_cl_popup_context=>rtti_get_data_element_texts( `/IWFND/SU_GWC_RH_VH`  )-medium
                          )->a( n = `contentWidth` v = `90%`
                          )->a( n = `afterClose` v = client->_event( 'F4_CLOSE' ) ).


    simple_form = dialog->ele( n = `SimpleForm` ns = `form`
                                )->a( n = `layout` v = `ResponsiveGridLayout`
                                )->a( n = `editable` b = abap_true
                                )->ele( n = `content` ns = `form` ).



    LOOP AT mt_dfies REFERENCE INTO dfies.

      IF dfies->fieldname = `MANDT`.
        CONTINUE.
      ENDIF.
      IF NOT ( dfies->keyflag = abap_true OR dfies->fieldname = mv_check_tab_field ).
        CONTINUE.
      ENDIF.


      ASSIGN ms_data_row->* TO <row>.


      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <row> TO <val>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      simple_form->tag( `Label`
          )->a( n = `text` v = z2ui5_cl_popup_context=>rtti_get_data_element_text_l( dfies->rollname ) ).

      simple_form->tag( `Input`
          )->a( n = `value` v = client->_bind( <val> )
          )->a( n = `showValueHelp` b = abap_false
          )->a( n = `submit` v = client->_event( 'F4_INPUT_DONE' ) ).

    ENDLOOP.

    simple_form->tag( `Label`
        )->a( n = `text` v = z2ui5_cl_popup_context=>rtti_get_data_element_text_l( 'SYST_TABIX' ) ).

    simple_form->tag( `Input`
        )->a( n = `value` v = client->_bind( mv_rows )
        )->a( n = `showValueHelp` b = abap_false
        )->a( n = `submit` v = client->_event( 'F4_INPUT_DONE' )
        )->a( n = `maxLength` v = `3` ).


    ASSIGN mt_data->* TO <table>.


    table = dialog->ele( `Table`
                      )->a( n = `growing`    v = `true`
                      )->a( n = `width`      v = `auto`
                      )->a( n = `items`      v = client->_bind( val = <table> )
                      )->a( n = `headerText` t = mv_check_tab ).


    header = table->ele( `headerToolbar`
                       )->ele( `OverflowToolbar`
                       )->tag( `Title`
                       )->a( n = `text` t = mv_check_tab
                       )->tag( `ToolbarSpacer` ).

    header = z2ui5_cl_layo_pop=>render_layout_function( xml    = header
                                                        client = client
                                                        layout = mo_layout ).

    render_columns( table  = table
                    client = client
                    layout = mo_layout
                    event  = `F4_ROW_SELECT` ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD on_event.

    DATA lv_value TYPE string.
    DATA lv_found TYPE abap_bool.
        DATA result TYPE string.

    CASE client->get( )-event.

      WHEN `F4_CLOSE`.

        client->popup_destroy( ).

        client->nav_app_leave( ).

      WHEN `F4_ROW_SELECT`.

        read_row_value( EXPORTING data  = mt_data
                                  t_arg = client->get( )-t_event_arg
                                  fname = mv_check_tab_field
                        IMPORTING value = lv_value
                                  found = lv_found ).
        IF lv_found = abap_false.
          RETURN.
        ENDIF.

        mv_return_value = lv_value.
        mv_check_confirmed = abap_true.

        client->popup_destroy( ).

        client->nav_app_leave( ).

      WHEN 'F4_INPUT_DONE'.


        result = z2ui5_cl_popup_context=>tab_get_where_by_dfies( mv_check_tab_field = mv_check_tab_field
                                                                  ms_data_row        = ms_data_row
                                                                  it_dfies           = mt_dfies ).

        get_data( result ).

      WHEN OTHERS.

        z2ui5_cl_layo_pop=>on_event_layout( client = client
                                            layout = mo_layout ).

    ENDCASE.

  ENDMETHOD.

  METHOD set_row_id.

    FIELD-SYMBOLS <tab>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE any.
      DATA lv_tabix LIKE sy-tabix.
      FIELD-SYMBOLS <row> TYPE any.

    ASSIGN mt_data->* TO <tab>.

    LOOP AT <tab> ASSIGNING <line>.


      lv_tabix = sy-tabix.


      ASSIGN COMPONENT 'ROW_ID' OF STRUCTURE <line> TO <row>.
      IF sy-subrc = 0.
        <row> = lv_tabix.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD factory.

    CREATE OBJECT result.

    result->mv_table = i_table.
    result->mv_field = i_fname.
    result->mv_value = i_value.

  ENDMETHOD.

  METHOD get_dfies.

    DATA t_dfies TYPE z2ui5_cl_popup_context=>ty_t_dfies.
    FIELD-SYMBOLS <dfies> TYPE z2ui5_cl_popup_context=>ty_s_dfies.
    DATA temp13 TYPE string.
    DATA temp14 TYPE string.
    DATA temp15 TYPE z2ui5_cl_popup_context=>ty_s_dfies.
      DATA temp16 TYPE string.
      DATA temp17 TYPE z2ui5_cl_popup_context=>ty_s_dfies.
    t_dfies = z2ui5_cl_popup_context=>rtti_get_t_dfies_by_table_name( mv_table ).


    READ TABLE t_dfies WITH KEY fieldname = mv_field ASSIGNING <dfies>.
    IF <dfies> IS NOT ASSIGNED.

      client->popup_destroy( ).
      client->nav_app_leave( ).
      RETURN.

    ENDIF.

    " a field without a check table has no values to offer - back to the
    " caller, as for an unknown field: staying here without a view left the
    " caller's screen on display with every event routed to this popup
    IF <dfies>-checktable IS INITIAL.
      client->nav_app_leave( ).
      RETURN.
    ENDIF.


    temp13 = <dfies>-checktable.
    mt_dfies = z2ui5_cl_popup_context=>rtti_get_t_dfies_by_table_name( temp13 ).

    " determine the field of the check table, first via the data element

    CLEAR temp14.

    READ TABLE mt_dfies INTO temp15 WITH KEY rollname = <dfies>-rollname.
    IF sy-subrc = 0.
      temp14 = temp15-fieldname.
    ENDIF.
    mv_check_tab_field = temp14.

    " as a fallback, try to find it via the domain
    IF mv_check_tab_field IS INITIAL.

      CLEAR temp16.

      READ TABLE mt_dfies INTO temp17 WITH KEY domname = <dfies>-domname.
      IF sy-subrc = 0.
        temp16 = temp17-fieldname.
      ENDIF.
      mv_check_tab_field = temp16.
    ENDIF.
    mv_check_tab = <dfies>-checktable.

  ENDMETHOD.

  METHOD prefill_inputs.

    DATA temp18 LIKE LINE OF mt_dfies.
    DATA dfies LIKE REF TO temp18.
      FIELD-SYMBOLS <row> TYPE data.
      FIELD-SYMBOLS <val> TYPE any.
    LOOP AT mt_dfies REFERENCE INTO dfies.

      IF NOT ( dfies->keyflag = abap_true OR dfies->fieldname = mv_check_tab_field ).
        CONTINUE.
      ENDIF.


      ASSIGN ms_data_row->* TO <row>.


      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <row> TO <val>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF dfies->fieldname = mv_check_tab_field.

        <val> = mv_value.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD on_after_layout.
    DATA layout TYPE REF TO z2ui5_cl_layo_manager.

    " only relevant when returning from another app - or on a restored draft
    IF client->check_on_navigated( ) = abap_false OR mv_check_tab IS INITIAL.
      RETURN.
    ENDIF.


    layout = get_layout_back( client ).
    IF layout IS BOUND.
      mo_layout = layout.
    ENDIF.
    " back from the layout popup, or a restored draft (browser Forward, a
    " reload) whose popup went with the page - show it (again)
    render_view( ).

  ENDMETHOD.

  METHOD get_layout.

    DATA class TYPE string.
    class = z2ui5_cl_popup_context=>rtti_get_classname_by_ref( me ).

    " the layout belongs to the check table whose rows it shows - under the
    " table of the field, the fields of one table with different check
    " tables shared one layout of different columns
    mo_layout = z2ui5_cl_layo_manager=>factory( control  = z2ui5_cl_layo_manager=>m_table
                                                data     = mt_data
                                                handle01 = class
                                                handle02 = mv_check_tab
                                                handle03 = 'F4'  ).

  ENDMETHOD.

  METHOD render_columns.

    DATA columns TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA temp19 LIKE LINE OF layout->ms_layout-t_layout.
    DATA position LIKE REF TO temp19.
      DATA lv_index LIKE sy-tabix.
      DATA temp20 TYPE string.
    DATA cells TYPE REF TO z2ui5_cl_ui5_view_builder.
    columns = table->ele( `columns` ).



    LOOP AT layout->ms_layout-t_layout REFERENCE INTO position.

      lv_index = sy-tabix.


      IF label_by_rollname = abap_true.
        temp20 = z2ui5_cl_popup_context=>rtti_get_data_element_text_l( position->rollname ).
      ELSE.
        temp20 = position->tlabel.
      ENDIF.
      columns->ele( `Column`
          )->a( n = `visible` v = client->_bind( val       = position->visible
                                                        tab       = layout->ms_layout-t_layout
                                                        tab_index = lv_index )
          )->a( n = `mergeDuplicates` v = client->_bind( val       = position->merge
                                                        tab       = layout->ms_layout-t_layout
                                                        tab_index = lv_index )
          )->a( n = `minScreenWidth` v = client->_bind( val       = position->width
                                                        tab       = layout->ms_layout-t_layout
                                                        tab_index = lv_index )
          )->tag( `Text`
          )->a( n = `text` v = temp20 ).

    ENDLOOP.


    cells = columns->end(
                      )->ele( `items`
                      )->ele( `ColumnListItem`
                      )->a( n = `vAlign` v = `Middle`
                      )->a( n = `type` v = `Navigation`
                      )->a( n = `press` v = client->_event( val   = event
                                                                    arg   = `${ROW_ID}` )
                      )->ele( `cells` ).

    LOOP AT layout->ms_layout-t_layout REFERENCE INTO position.

      cells->ele( `ObjectIdentifier`
          )->a( n = `text` v = |\{{ z2ui5_cl_ui5_view_builder=>component_path( position->fname ) }\}| ).

    ENDLOOP.

  ENDMETHOD.

  METHOD read_row_value.

    FIELD-SYMBOLS <tab>   TYPE STANDARD TABLE.
    FIELD-SYMBOLS <row>   TYPE any.
    FIELD-SYMBOLS <value> TYPE any.
    DATA temp21 TYPE string.
    DATA temp22 TYPE string.
    DATA lv_arg LIKE temp21.
    DATA temp4 TYPE i.

    CLEAR: value, found.

    " the index comes from the client: an event without it raised
    " CX_SY_ITAB_LINE_NOT_FOUND, one that is no number a conversion error,
    " and a row that is not there must not reach ASSIGN COMPONENT

    CLEAR temp21.

    READ TABLE t_arg INTO temp22 INDEX 1.
    IF sy-subrc = 0.
      temp21 = temp22.
    ENDIF.

    lv_arg = temp21.
    IF lv_arg IS INITIAL OR lv_arg CN `0123456789` OR strlen( lv_arg ) > 9.
      RETURN.
    ENDIF.

    ASSIGN data->* TO <tab>.

    temp4 = lv_arg.
    READ TABLE <tab> INDEX temp4 ASSIGNING <row>.
    IF <row> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    ASSIGN COMPONENT fname OF STRUCTURE <row> TO <value>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    value = <value>.
    found = abap_true.

  ENDMETHOD.

  METHOD get_layout_back.
        DATA temp23 TYPE REF TO z2ui5_cl_layo_pop.
        DATA app LIKE temp23.

    " only relevant when returning from another app
    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.
        " the previous app was the layout popup - or any other, then nothing

        temp23 ?= client->get_app( client->get( )-s_draft-id_prev_app ).

        app = temp23.
        result = app->mo_layout.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

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
