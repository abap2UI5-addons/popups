CLASS z2ui5_cl_popup_sel_layout DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    " abap2ui5lint-disable unbound-public-attribute -- a popup's attributes are its interface to the app that called it, which reads them once the popup returns
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_result,
        row             TYPE REF TO data,
        table           TYPE REF TO data,
        check_confirmed TYPE abap_bool,
      END OF ty_s_result.

    DATA ms_result           TYPE ty_s_result.
    DATA mr_tab              TYPE REF TO data.
    DATA mr_tab_popup        TYPE REF TO data.
    DATA mr_tab_popup_backup TYPE REF TO data.
    DATA mo_layout           TYPE REF TO z2ui5_cl_layo_manager.
    DATA mv_search_value     TYPE string.

    "! Select one or several rows of any table in a popup whose columns
    "! follow a saved layout of layout-management (column choice, order,
    "! width, sorting). Layouts are kept per popup class and line type of
    "! i_tab.
    CLASS-METHODS factory
      IMPORTING
        i_tab              TYPE STANDARD TABLE
        i_title            TYPE clike     OPTIONAL
        i_contentwidth     TYPE clike     OPTIONAL
        i_contentheight    TYPE clike     OPTIONAL
        i_growingthreshold TYPE clike     OPTIONAL
        i_multiselect      TYPE abap_bool OPTIONAL
        i_event_canceled   TYPE string    OPTIONAL
        i_event_confirmed  TYPE string    OPTIONAL
      RETURNING
        VALUE(r_result)    TYPE REF TO z2ui5_cl_popup_sel_layout.

    METHODS result
      RETURNING
        VALUE(result) TYPE ty_s_result.
    " abap2ui5lint-enable unbound-public-attribute

  PROTECTED SECTION.
    CONSTANTS cv_sel_field    TYPE string VALUE `ZZSELKZ`.
    CONSTANTS cv_row_id_field TYPE string VALUE `ZZROW_ID`.

    DATA client            TYPE REF TO z2ui5_if_client.
    DATA check_table_line  TYPE abap_bool.
    DATA title             TYPE string.
    DATA content_width     TYPE string.
    DATA content_height    TYPE string.
    DATA growing_threshold TYPE string.
    DATA multiselect       TYPE abap_bool.
    DATA event_confirmed   TYPE string.
    DATA event_canceled    TYPE string.

    METHODS on_init.
    METHODS on_navigated.
    METHODS on_event.
    METHODS display.
    METHODS set_output_table.
    METHODS init_layout.
    METHODS sort_output_table.
    METHODS sync_selection.
    METHODS on_event_confirm.
    METHODS on_event_search.

  PRIVATE SECTION.
    METHODS get_line_type_name
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_search_fields
      RETURNING
        VALUE(result) TYPE string_table.

ENDCLASS.


CLASS z2ui5_cl_popup_sel_layout IMPLEMENTATION.

  METHOD factory.

    r_result = NEW #( ).
    r_result->title = COND #(
      WHEN i_title IS NOT INITIAL THEN i_title
      WHEN i_multiselect = abap_true THEN `Multi Select`
      ELSE `Single Select` ).

    r_result->content_width     = COND #( WHEN i_contentwidth IS NOT INITIAL THEN i_contentwidth ELSE `90%` ).
    r_result->content_height    = COND #( WHEN i_contentheight IS NOT INITIAL THEN i_contentheight ELSE `70%` ).
    r_result->growing_threshold = COND #( WHEN i_growingthreshold IS NOT INITIAL THEN i_growingthreshold ELSE `100` ).
    r_result->multiselect       = i_multiselect.
    r_result->event_confirmed   = i_event_confirmed.
    r_result->event_canceled    = i_event_canceled.

    r_result->mr_tab = z2ui5_cl_popup_context=>conv_copy_ref_data( i_tab ).
    CREATE DATA r_result->ms_result-row LIKE LINE OF i_tab.
    CREATE DATA r_result->ms_result-table LIKE i_tab.

  ENDMETHOD.

  METHOD result.

    result = ms_result.

  ENDMETHOD.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ).
      on_init( ).
    ELSEIF client->check_on_navigated( ).
      on_navigated( ).
    ELSE.
      on_event( ).
    ENDIF.

  ENDMETHOD.

  METHOD on_init.

    set_output_table( ).
    init_layout( ).
    sort_output_table( ).
    display( ).

  ENDMETHOD.

  METHOD on_navigated.

    " back from the layout popup - take over the layout it edited or loaded
    TRY.
        DATA(layout_popup) = CAST z2ui5_cl_layo_pop( client->get_app_prev( ) ).
        mo_layout = layout_popup->mo_layout.
      CATCH cx_root ##NO_HANDLER.
        " not the layout popup (e.g. a restored draft) - keep the layout
    ENDTRY.

    sort_output_table( ).
    display( ).

  ENDMETHOD.

  METHOD on_event.

    CASE client->get( )-event.

      WHEN `CONFIRM`.
        on_event_confirm( ).

      WHEN `SELECTION_CHANGE`.
        " a single selection confirms at once, a multi selection waits for OK
        IF multiselect = abap_false.
          on_event_confirm( ).
        ENDIF.

      WHEN `CANCEL`.
        client->popup_destroy( ).
        client->nav_app_leave( event = event_canceled ).

      WHEN `SEARCH`.
        on_event_search( ).

      WHEN OTHERS.
        z2ui5_cl_layo_pop=>on_event_layout( client = client
                                            layout = mo_layout ).

    ENDCASE.

  ENDMETHOD.

  METHOD set_output_table.

    DATA lr_row   TYPE REF TO data.
    DATA lv_index TYPE i.

    FIELD-SYMBOLS <tab>      TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_out>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_out2> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <row>      TYPE any.
    FIELD-SYMBOLS <row2>     TYPE any.
    FIELD-SYMBOLS <field>    TYPE any.

    ASSIGN mr_tab->* TO <tab>.

    " the components of the line (an elementary line becomes TAB_LINE) plus
    " the selection flag and a row id that survives searching and sorting
    DATA(ls_sel_tab_type) = z2ui5_cl_popup_context=>rtti_create_sel_tab_type( ir_tab         = mr_tab
                                                                             add_sel_field  = abap_true
                                                                             sel_field_name = cv_sel_field ).
    check_table_line = ls_sel_tab_type-check_table_line.

    DATA(lo_line) = CAST cl_abap_structdescr( ls_sel_tab_type-tabledescr->get_table_line_type( ) ).
    DATA(lt_comp) = lo_line->get_components( ).
    IF NOT line_exists( lt_comp[ name = cv_row_id_field ] ).
      INSERT VALUE #( name = cv_row_id_field
                      type = CAST #( cl_abap_datadescr=>describe_by_data( lv_index ) ) ) INTO TABLE lt_comp.
    ENDIF.
    DATA(lo_tabledescr) = cl_abap_tabledescr=>create( cl_abap_structdescr=>create( lt_comp ) ).

    CREATE DATA mr_tab_popup TYPE HANDLE lo_tabledescr.
    CREATE DATA mr_tab_popup_backup TYPE HANDLE lo_tabledescr.

    ASSIGN mr_tab_popup->* TO <tab_out>.
    ASSIGN mr_tab_popup_backup->* TO <tab_out2>.

    LOOP AT <tab> ASSIGNING <row>.
      lv_index = sy-tabix.

      CREATE DATA lr_row LIKE LINE OF <tab_out>.
      ASSIGN lr_row->* TO <row2>.
      IF check_table_line = abap_true.
        ASSIGN COMPONENT `TAB_LINE` OF STRUCTURE <row2> TO <field>.
        ASSERT sy-subrc = 0.
        <field> = <row>.
      ELSE.
        MOVE-CORRESPONDING <row> TO <row2>.
      ENDIF.

      ASSIGN COMPONENT cv_row_id_field OF STRUCTURE <row2> TO <field>.
      ASSERT sy-subrc = 0.
      <field> = lv_index.

      INSERT <row2> INTO TABLE <tab_out>.
    ENDLOOP.

    " the backup keeps every row in its original order: the row id is its index
    <tab_out2> = <tab_out>.

  ENDMETHOD.

  METHOD init_layout.

    DATA lr_layout_tab TYPE REF TO data.

    " the layout is built on the columns of the caller's line only - the
    " selection flag and the row id are no columns to choose
    DATA(ls_type) = z2ui5_cl_popup_context=>rtti_create_sel_tab_type( mr_tab ).
    CREATE DATA lr_layout_tab TYPE HANDLE ls_type-tabledescr.

    mo_layout = z2ui5_cl_layo_manager=>factory( control  = z2ui5_cl_layo_manager=>m_table
                                                data     = lr_layout_tab
                                                handle01 = z2ui5_cl_popup_context=>rtti_get_classname_by_ref( me )
                                                handle02 = get_line_type_name( )
                                                handle03 = ``
                                                handle04 = `` ).

  ENDMETHOD.

  METHOD get_line_type_name.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

    ASSIGN mr_tab->* TO <tab>.

    DATA(lo_table) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( <tab> ) ).
    result = lo_table->get_table_line_type( )->get_relative_name( ).

  ENDMETHOD.

  METHOD get_search_fields.

    LOOP AT mo_layout->ms_layout-t_layout INTO DATA(ls_layout).
      INSERT CONV string( ls_layout-fname ) INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD sort_output_table.

    " the sorting of the layout - point it at the table again first: the
    " draft restores the layout's own reference as one to a copy
    mo_layout->mr_data = mr_tab_popup.
    mo_layout->sort( abap_true ).

  ENDMETHOD.

  METHOD sync_selection.

    FIELD-SYMBOLS <tab_out>        TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_out_backup> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <row>            TYPE any.
    FIELD-SYMBOLS <row_backup>     TYPE any.
    FIELD-SYMBOLS <row_id>         TYPE any.
    FIELD-SYMBOLS <selkz>          TYPE any.
    FIELD-SYMBOLS <selkz_backup>   TYPE any.

    ASSIGN mr_tab_popup->* TO <tab_out>.
    ASSIGN mr_tab_popup_backup->* TO <tab_out_backup>.

    " a single selection replaces one hidden by a search
    IF multiselect = abap_false.
      LOOP AT <tab_out_backup> ASSIGNING <row_backup>.
        ASSIGN COMPONENT cv_sel_field OF STRUCTURE <row_backup> TO <selkz_backup>.
        ASSERT sy-subrc = 0.
        CLEAR <selkz_backup>.
      ENDLOOP.
    ENDIF.

    LOOP AT <tab_out> ASSIGNING <row>.
      ASSIGN COMPONENT cv_row_id_field OF STRUCTURE <row> TO <row_id>.
      ASSERT sy-subrc = 0.
      ASSIGN COMPONENT cv_sel_field OF STRUCTURE <row> TO <selkz>.
      ASSERT sy-subrc = 0.

      READ TABLE <tab_out_backup> ASSIGNING <row_backup> INDEX <row_id>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT cv_sel_field OF STRUCTURE <row_backup> TO <selkz_backup>.
      ASSERT sy-subrc = 0.
      <selkz_backup> = <selkz>.
    ENDLOOP.

  ENDMETHOD.

  METHOD display.

    FIELD-SYMBOLS <tab_out> TYPE STANDARD TABLE.

    ASSIGN mr_tab_popup->* TO <tab_out>.

    DATA(popup) = z2ui5_cl_ui5_view_builder=>factory(
                      )->ele( n = `FragmentDefinition` ns = `core`
                      )->a( n = `xmlns` v = `sap.m`
                      )->a( n = `xmlns:core` v = `sap.ui.core` ).

    DATA(dialog) = popup->ele( `Dialog`
                       )->a( n = `title` t = title
                       )->a( n = `contentWidth` v = content_width
                       )->a( n = `contentHeight` v = content_height
                       )->a( n = `afterClose` v = client->_event( `CANCEL` ) ).

    DATA(table) = dialog->ele( `content`
                       )->ele( `Table`
                       )->a( n = `items` v = client->_bind( <tab_out> )
                       )->a( n = `mode` v = COND #( WHEN multiselect = abap_true
                                                    THEN `MultiSelect`
                                                    ELSE `SingleSelectLeft` )
                       )->a( n = `growing` b = abap_true
                       )->a( n = `growingThreshold` v = growing_threshold
                       )->a( n = `width` v = `auto`
                       )->a( n = `selectionChange` v = client->_event( `SELECTION_CHANGE` ) ).

    DATA(toolbar) = table->ele( `headerToolbar`
                        )->ele( `OverflowToolbar`
                        )->tag( `ToolbarSpacer`
                        )->tag( `SearchField`
                        )->a( n = `width` v = `17.5rem`
                        )->a( n = `value` v = client->_bind( mv_search_value )
                        )->a( n = `search` v = client->_event( `SEARCH` ) ).

    z2ui5_cl_layo_pop=>render_layout_function( xml    = toolbar
                                               client = client
                                               layout = mo_layout ).

    DATA(columns) = table->ele( `columns` ).

    LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO DATA(layout).
      DATA(lv_index) = sy-tabix.

      columns->ele( `Column`
          )->a( n = `width` v = client->_bind( val       = layout->width
                                               tab       = mo_layout->ms_layout-t_layout
                                               tab_index = lv_index )
          )->a( n = `visible` v = client->_bind( val       = layout->visible
                                                 tab       = mo_layout->ms_layout-t_layout
                                                 tab_index = lv_index )
          )->a( n = `mergeDuplicates` v = client->_bind( val       = layout->merge
                                                         tab       = mo_layout->ms_layout-t_layout
                                                         tab_index = lv_index )
          )->tag( `Text`
          )->a( n = `text` t = layout->tlabel ).

    ENDLOOP.

    DATA(cells) = table->ele( `items`
                      )->ele( `ColumnListItem`
                      )->a( n = `vAlign` v = `Middle`
                      )->a( n = `selected` v = |\{{ cv_sel_field }\}|
                      )->ele( `cells` ).

    LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO layout.

      " a reference field (e.g. the unit of a quantity) is shown in the cell
      DATA(cell_text) = |\{{ layout->fname }\}|.
      IF layout->reference_field IS NOT INITIAL
          AND line_exists( mo_layout->ms_layout-t_layout[ fname = layout->reference_field ] ).
        cell_text = |{ cell_text } \{{ layout->reference_field }\}|.
      ENDIF.

      cells->tag( `Text`
          )->a( n = `text` v = cell_text ).

    ENDLOOP.

    dialog->ele( `buttons`
        )->tag( `Button`
        )->a( n = `text` v = `Cancel`
        )->a( n = `press` v = client->_event( `CANCEL` )
        )->tag( `Button`
        )->a( n = `text` v = `OK`
        )->a( n = `press` v = client->_event( `CONFIRM` )
        )->a( n = `type` v = `Emphasized` ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD on_event_confirm.

    FIELD-SYMBOLS <tab_backup>          TYPE STANDARD TABLE.
    FIELD-SYMBOLS <table_result>        TYPE ANY TABLE.
    FIELD-SYMBOLS <row_selected>        TYPE any.
    FIELD-SYMBOLS <selkz>               TYPE any.
    FIELD-SYMBOLS <row_result>          TYPE any.
    FIELD-SYMBOLS <table_line_selected> TYPE any.

    sync_selection( ).

    ASSIGN mr_tab_popup_backup->* TO <tab_backup>.
    ASSIGN ms_result-table->* TO <table_result>.
    ASSIGN ms_result-row->* TO <row_result>.

    " the result only contains rows of this confirmation, in the caller's order
    CLEAR <table_result>.

    LOOP AT <tab_backup> ASSIGNING <row_selected>.

      ASSIGN COMPONENT cv_sel_field OF STRUCTURE <row_selected> TO <selkz>.
      ASSERT sy-subrc = 0.
      IF <selkz> = abap_false.
        CONTINUE.
      ENDIF.

      IF check_table_line = abap_true.
        ASSIGN COMPONENT `TAB_LINE` OF STRUCTURE <row_selected> TO <table_line_selected>.
        ASSERT sy-subrc = 0.
        <row_result> = <table_line_selected>.
      ELSE.
        CLEAR <row_result>.
        MOVE-CORRESPONDING <row_selected> TO <row_result>.
      ENDIF.

      INSERT <row_result> INTO TABLE <table_result>.
      IF multiselect = abap_false.
        EXIT.
      ENDIF.

    ENDLOOP.

    ms_result-check_confirmed = abap_true.

    client->popup_destroy( ).
    client->nav_app_leave( event  = event_confirmed
                           r_data = <table_result> ).

  ENDMETHOD.

  METHOD on_event_search.

    FIELD-SYMBOLS <tab_out>        TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_out_backup> TYPE STANDARD TABLE.

    " keep what was selected before the rows are filtered anew
    sync_selection( ).

    ASSIGN mr_tab_popup->* TO <tab_out>.
    ASSIGN mr_tab_popup_backup->* TO <tab_out_backup>.

    <tab_out> = <tab_out_backup>.

    z2ui5_cl_popup_context=>itab_filter_by_val( EXPORTING val         = mv_search_value
                                                          fields      = get_search_fields( )
                                                          ignore_case = abap_true
                                                CHANGING  tab         = <tab_out> ).

    sort_output_table( ).

  ENDMETHOD.

ENDCLASS.
