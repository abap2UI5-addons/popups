CLASS z2ui5_cl_pop_search_help DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_serializable_object.
    INTERFACES z2ui5_if_app.

    DATA mv_table        TYPE string.
    DATA mv_fname        TYPE string.
    DATA mv_shlpfield    TYPE string.
    DATA mv_value        TYPE string.
    DATA mv_return_value TYPE string.
    DATA mv_rows         TYPE int1 VALUE '50'.
    DATA mt_data         TYPE REF TO data.
    DATA ms_data_row     TYPE REF TO data.
    DATA mo_layout       TYPE REF TO z2ui5_cl_layo_manager.
    DATA ms_shlp         TYPE shlp_descr.
    DATA mt_result_desc  TYPE TABLE OF dfies.
    DATA mr_data         TYPE REF TO data.

    CLASS-METHODS factory
      IMPORTING
        i_table       TYPE string
        i_fname       TYPE string
        i_value       TYPE string
        i_data        TYPE REF TO data OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_pop_search_help.

  PROTECTED SECTION.
    TYPES ty_return_tab   TYPE STANDARD TABLE OF ddshretval WITH DEFAULT KEY.
    TYPES ty_recdescr_tab TYPE STANDARD TABLE OF dfies WITH DEFAULT KEY.

    DATA client  TYPE REF TO z2ui5_if_client.
    DATA mv_init TYPE abap_bool.

    METHODS on_init.

    METHODS render_view.

    METHODS on_event.

    METHODS set_row_id.

    METHODS get_txt
      IMPORTING
        roll          TYPE string
      RETURNING
        VALUE(result) TYPE z2ui5_cl_util_abap=>ty_s_data_element_text.

    METHODS get_txt_l
      IMPORTING
        roll          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_comp
      RETURNING
        VALUE(result) TYPE abap_component_tab.

    METHODS on_after_layout.

    METHODS get_search_help_data.

    METHODS get_layout.

    METHODS set_selopt.

PRIVATE SECTION.
    METHODS set_init_selopt.
ENDCLASS.


CLASS z2ui5_cl_pop_search_help IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF mv_init = abap_false.
      mv_init = abap_true.

      on_init( ).

      render_view( ).

    ENDIF.

    on_event( ).

    on_after_layout( ).

  ENDMETHOD.

  METHOD on_init.

    get_search_help_data( ).

    get_layout( ).

  ENDMETHOD.

  METHOD get_layout.

    DATA(class) = cl_abap_classdescr=>get_class_name( me ).
    SHIFT class LEFT DELETING LEADING '\CLASS='.
    DATA(app) = z2ui5_cl_util=>url_param_get( val = 'app'
                                              url = client->get( )-s_config-search ).

    mo_layout = z2ui5_cl_layo_manager=>factory( control  = z2ui5_cl_layo_manager=>m_table
                                          data     = mt_data
                                          handle01 = class
                                          handle02 = mv_shlpfield
                                          handle03 = app
                                          handle04 = ``  ).

  ENDMETHOD.

  METHOD render_view.

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup( ).

    DATA(simple_form) = popup->dialog( title        = 'Search-Help'
                                       contentwidth = '70%'
                                       afterclose   = client->_event( 'SHLP_CLOSE' )
          )->simple_form( title    = 'Search-Help'
                          layout   = 'ResponsiveGridLayout'
                          editable = abap_true
          )->content( ns = 'form' ).

    " Gehe über alle Comps
    LOOP AT mt_result_desc REFERENCE INTO DATA(dfies).

      " Fixed Value in Searchhelp
      IF VALUE #( ms_shlp-interface[ shlpfield = dfies->fieldname ]-value OPTIONAL ) IS INITIAL.
        DATA(enabled) = abap_true.
      ELSE.
        enabled = abap_false.
      ENDIF.

      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE ms_data_row->* TO FIELD-SYMBOL(<val>).

      simple_form->label( text = get_txt( CONV #( dfies->rollname ) )-long ).

      simple_form->input( value         = client->_bind_edit( <val> )
                          showvaluehelp = abap_false
                          submit        = client->_event( 'SHLP_INPUT_DONE' )
                          enabled       = enabled ).

    ENDLOOP.

    DATA(table) = popup->get_child( )->table( growing    = 'true'
                                              width      = 'auto'
                                              items      = client->_bind( val = mt_data->* )
                                              headertext = Z2UI5_CL_UTIL_ABAP=>rtti_get_table_desrc( mv_table ) ).

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(headder) = table->header_toolbar(
                 )->overflow_toolbar(
                 )->title( text = Z2UI5_CL_UTIL_ABAP=>rtti_get_table_desrc( mv_table )
                 )->toolbar_spacer( ).

    headder = z2ui5_cl_layo_pop=>render_layout_function( xml    = headder
                                                                   client = client
                                                                   layout = mo_layout ).

    DATA(columns) = table->columns( ).

    LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO DATA(layout).
      DATA(lv_index) = sy-tabix.

      columns->column( visible         = client->_bind( val       = layout->visible
                                                        tab       = mo_layout->ms_layout-t_layout
                                                        tab_index = lv_index )
*                       halign          = client->_bind( val       = layout->halign
*                                                        tab       = mo_layout->ms_layout-t_layout
*                                                        tab_index = lv_index )
*                       importance      = client->_bind( val       = layout->importance
*                                                        tab       = mo_layout->ms_layout-t_layout
*                                                        tab_index = lv_index )
                       mergeduplicates = client->_bind( val       = layout->merge
                                                        tab       = mo_layout->ms_layout-t_layout
                                                        tab_index = lv_index )
                       minscreenwidth  = client->_bind( val       = layout->width
                                                        tab       = mo_layout->ms_layout-t_layout
                                                        tab_index = lv_index )
       )->text( get_txt( CONV #( layout->rollname ) )-long ).

    ENDLOOP.

    DATA(cells) = columns->get_parent( )->items(
                                       )->column_list_item(
                                           valign = 'Middle'
                                           type   = 'Navigation'
                                           press  = client->_event( val   = 'SHLP_ROW_SELECT'
                                                                    t_arg = VALUE #( ( `${ROW_ID}`  ) ) )
                                       )->cells( ).

    LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO layout.

      cells->object_identifier( text = |\{{ layout->fname }\}| ).

    ENDLOOP.

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD on_event.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

    CASE client->get( )-event.

      WHEN `SHLP_CLOSE`.

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN `SHLP_ROW_SELECT`.

        DATA(lt_arg) = client->get( )-t_event_arg.

        ASSIGN mt_data->* TO <tab>.

        ASSIGN <tab>[ lt_arg[ 1 ] ] TO FIELD-SYMBOL(<row>).

        ASSIGN COMPONENT mv_shlpfield OF STRUCTURE <row> TO FIELD-SYMBOL(<value>).

        mv_return_value = <value>.

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'SHLP_INPUT_DONE'.

        set_selopt( ).

        get_search_help_data( ).

        client->popup_model_update( ).

      WHEN OTHERS.

        z2ui5_cl_layo_pop=>on_event_layout( client = client
                                                      layout = mo_layout ).

    ENDCASE.

  ENDMETHOD.

  METHOD set_row_id.

    FIELD-SYMBOLS <tab>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE any.

    ASSIGN mt_data->* TO <tab>.

    LOOP AT <tab> ASSIGNING <line>.

      ASSIGN COMPONENT 'ROW_ID' OF STRUCTURE <line> TO FIELD-SYMBOL(<row>).
      IF <row> IS ASSIGNED.
        <row> = sy-tabix.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD factory.

    result = NEW #( ).

    result->mv_table = i_table.
    result->mv_fname = i_fname.
    result->mv_value = i_value.

    IF i_data IS SUPPLIED.

      DATA(t_comp) = z2ui5_cl_util=>rtti_get_t_attri_by_any( i_data ).

      DATA(struct_desc) = cl_abap_structdescr=>create( t_comp ).

      CREATE DATA result->mr_data TYPE HANDLE struct_desc.

      result->mr_data->* = i_data->*.

    ENDIF.

  ENDMETHOD.

  METHOD get_txt.

    result = z2ui5_cl_util=>rtti_get_data_element_texts(  roll  ).

  ENDMETHOD.

  METHOD get_txt_l.

    result = z2ui5_cl_util=>rtti_get_data_element_texts( roll )-long.

  ENDMETHOD.

  METHOD on_after_layout.

    " Kommen wir aus einer anderen APP
    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.
        " War es das Layout?
        DATA(app) = CAST z2ui5_cl_layo_pop( client->get_app( client->get( )-s_draft-id_prev_app ) ).

        mo_layout = app->mo_layout.

        render_view( ).

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD get_search_help_data.

    DATA lt_shlp       TYPE shlp_desct.
    DATA lt_result_tab TYPE TABLE OF string.
    DATA ls_comp       TYPE abap_componentdescr.
    DATA lt_comps      TYPE abap_component_tab.
    DATA lo_datadescr  TYPE REF TO cl_abap_datadescr.
    DATA lr_line       TYPE REF TO data.

    IF ms_shlp IS INITIAL.
      " Suchhilfe lesen
      CALL FUNCTION 'F4IF_DETERMINE_SEARCHHELP'
        EXPORTING
          tabname           = CONV tabname( mv_table )
          fieldname         = CONV fieldname( mv_fname )
        IMPORTING
          shlp              = ms_shlp
        EXCEPTIONS
          field_not_found   = 1
          no_help_for_field = 2
          inconsistent_help = 3
          OTHERS            = 4.
      IF sy-subrc <> 0.
        " FEHLER
      ENDIF.

      IF ms_shlp-intdescr-issimple = abap_false.
        CALL FUNCTION 'F4IF_EXPAND_SEARCHHELP'
          EXPORTING
            shlp_top = ms_shlp
          IMPORTING
            shlp_tab = lt_shlp.

        ms_shlp = VALUE #( lt_shlp[ 1 ] OPTIONAL ).
      ENDIF.
    ENDIF.

    IF mr_data IS BOUND.
      " Values from Caller app to Interface Values
      LOOP AT ms_shlp-interface REFERENCE INTO DATA(r_interface) WHERE value IS INITIAL.

        ASSIGN COMPONENT r_interface->shlpfield OF STRUCTURE mr_data->* TO FIELD-SYMBOL(<value>).

        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        r_interface->value = <value>.

      ENDLOOP.
    ENDIF.

    " Interface Fixed Values to Selopt
    LOOP AT ms_shlp-interface INTO DATA(interface).

      " Match the name of the SH Field to the Input field name
      IF interface-valfield = mv_fname.
        mv_shlpfield = interface-shlpfield.
      ENDIF.

      IF interface-value IS NOT INITIAL.

        ms_shlp-selopt = VALUE #( BASE ms_shlp-selopt
                                  ( shlpfield = interface-shlpfield
                                    shlpname  = interface-valtabname
                                    option    = COND #( WHEN interface-value CA `*` THEN 'CP' ELSE 'EQ' )
                                    sign      = 'I'
                                    low       = interface-value  ) ).

      ENDIF.

    ENDLOOP.

    LOOP AT ms_shlp-fieldprop INTO DATA(fieldrop).

      IF fieldrop-defaultval IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(valule) = fieldrop-defaultval.
      REPLACE ALL OCCURRENCES OF `'` IN valule WITH ``.

      ms_shlp-selopt = VALUE #( BASE ms_shlp-selopt
                                ( shlpfield = fieldrop-fieldname
*                                  shlpname  =
                                  option    = COND #( WHEN fieldrop-defaultval CA `*` THEN 'CP' ELSE 'EQ' )
                                  sign      = 'I'
                                  low       = valule  ) ).

    ENDLOOP.

    CALL FUNCTION 'F4IF_SELECT_VALUES'
      EXPORTING
        shlp           = ms_shlp
        sort           = space
        call_shlp_exit = abap_true
      TABLES
        record_tab     = lt_result_tab
        recdescr_tab   = mt_result_desc.

    SORT ms_shlp-fieldprop BY shlplispos ASCENDING.

    LOOP AT ms_shlp-fieldprop INTO DATA(field_props) WHERE shlplispos IS NOT INITIAL.

      DATA(descption) = VALUE #( mt_result_desc[ fieldname = field_props-fieldname ] OPTIONAL ).

      ls_comp-name  = descption-fieldname.
      ls_comp-type ?= cl_abap_datadescr=>describe_by_name( descption-rollname ).
      APPEND ls_comp TO lt_comps.

    ENDLOOP.

    IF NOT line_exists( lt_comps[ name = 'ROW_ID' ] ).
      lo_datadescr ?= cl_abap_datadescr=>describe_by_name( 'INT4' ).
      ls_comp-name  = 'ROW_ID'.
      ls_comp-type ?= lo_datadescr.
      APPEND ls_comp TO lt_comps.
    ENDIF.

    DATA(strucdescr) = cl_abap_structdescr=>create( p_components = lt_comps ).

    DATA(tabdescr) = cl_abap_tabledescr=>create( p_line_type = strucdescr ).

    IF mt_data IS NOT BOUND.
      CREATE DATA mt_data TYPE HANDLE tabdescr.
    ENDIF.

    ASSIGN mt_data->* TO FIELD-SYMBOL(<fs_target_tab>).

    CLEAR <fs_target_tab>.

    " we dont want to loose all inputs in row ...
    IF ms_data_row IS NOT BOUND.
      CREATE DATA ms_data_row TYPE HANDLE strucdescr.
    ENDIF.

    LOOP AT lt_result_tab INTO DATA(result_line).

      CREATE DATA lr_line TYPE HANDLE strucdescr.
      ASSIGN lr_line->* TO FIELD-SYMBOL(<fs_line>).

      LOOP AT mt_result_desc INTO DATA(result_desc).

        ASSIGN COMPONENT result_desc-fieldname OF STRUCTURE <fs_line>
               TO FIELD-SYMBOL(<line_content>).

        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF result_desc-leng < result_desc-intlen.
          " interne Darstellung anders als externe Darstellung
          " UNICODE, offset halbieren
          result_desc-offset /= 2.
        ENDIF.

        TRY.
            <line_content> = result_line+result_desc-offset(result_desc-outputlen).
          CATCH cx_root.

             Try.
            " Sting table will crash if value length <> outputlen
            <line_content> = result_line+result_desc-offset.
            catch cx_root.
            " rest of the fields are empty.
            ENDTRY.
        ENDTRY.

      ENDLOOP.

      INSERT <fs_line> INTO TABLE <fs_target_tab>.

    ENDLOOP.

    " Set default values
    LOOP AT ms_shlp-interface INTO interface.

      IF interface-value IS NOT INITIAL.

        ASSIGN COMPONENT interface-shlpfield OF STRUCTURE ms_data_row->* TO <value>.

        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        <value> = interface-value.

      ENDIF.

    ENDLOOP.

*    LOOP AT ms_shlp-fieldprop INTO fieldrop.
*
*      IF fieldrop-defaultval IS NOT INITIAL.
*
*        ASSIGN COMPONENT fieldrop-fieldname OF STRUCTURE ms_data_row->* TO <value>.
*
*        IF sy-subrc <> 0.
*          CONTINUE.
*        ENDIF.
*        <value> = fieldrop-defaultval.
*       REPLACE ALL OCCURRENCES OF `'` in <value> with ``.
*      ENDIF.
*
*    ENDLOOP.

    set_row_id( ).

  ENDMETHOD.

  METHOD get_comp.

    DATA index TYPE int4.

    TRY.

        DATA(comp) = z2ui5_cl_util=>rtti_get_t_attri_by_table_name( mv_table ).

        result = VALUE cl_abap_structdescr=>component_table(
                           ( name = 'ROW_ID'
                             type = CAST #( cl_abap_datadescr=>describe_by_data( index ) ) ) ).

        APPEND LINES OF comp TO result.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD set_selopt.

    CLEAR ms_shlp-selopt.

    LOOP AT mt_result_desc INTO DATA(dfies).

      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE ms_data_row->* TO FIELD-SYMBOL(<value>).

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      IF <value> IS INITIAL.
        CONTINUE.
      ENDIF.

      ms_shlp-selopt = VALUE #( BASE ms_shlp-selopt
                                ( shlpfield = dfies-fieldname
                                  shlpname  = ''
                                  sign      = 'I'
                                  option    = 'CP'
                                  low       = |*{ <value> }*|  ) ).

    ENDLOOP.

  ENDMETHOD.

  METHOD set_init_selopt.

    CLEAR ms_shlp-selopt.

    LOOP AT mt_result_desc INTO DATA(dfies).

      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE mr_data->* TO FIELD-SYMBOL(<value>).

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      IF <value> IS INITIAL.
        CONTINUE.
      ENDIF.

      ms_shlp-selopt = VALUE #( BASE ms_shlp-selopt
                                ( shlpfield = dfies-fieldname
                                  shlpname  = ''
                                  sign      = 'I'
                                  option    = 'CP'
                                  low       = |*{ <value> }*|  ) ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
