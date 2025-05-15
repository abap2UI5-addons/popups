CLASS z2ui5_cl_util_abap_new DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES trobj_name     TYPE c LENGTH 120.
    TYPES sxco_transport TYPE c LENGTH 20.
    TYPES:
      BEGIN OF ty_s_transport,
        short_description TYPE string,
        transport         TYPE sxco_transport,
        task              TYPE sxco_transport,
        selkz             TYPE abap_bool,
      END OF ty_s_transport.

    TYPES ty_t_data TYPE STANDARD TABLE OF ty_s_transport WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_dfies_2,
        tabname     TYPE c LENGTH 30,    " Table Name
        fieldname   TYPE c LENGTH 30,    " Field Name
        langu       TYPE c LENGTH 1,     " Language Key
        position    TYPE n LENGTH 4,     " Position of the field in the table
        offset      TYPE n LENGTH 6,     " Offset of a field
        domname     TYPE c LENGTH 30,    " Domain name
        rollname    TYPE c LENGTH 30,    " Data element (semantic domain)
        checktable  TYPE c LENGTH 30,    " Check Table
        leng        TYPE n LENGTH 6,     " Length (Characters)
        intlen      TYPE n LENGTH 6,     " Internal Length (Bytes)
        outputlen   TYPE n LENGTH 6,     " Output Length
        decimals    TYPE n LENGTH 6,     " Number of Decimal Places
        datatype    TYPE c LENGTH 4,     " Dynpro Data Type
        inttype     TYPE c LENGTH 1,     " ABAP Data Type (C,D,N,...)
        reftable    TYPE c LENGTH 30,    " Reference Table
        reffield    TYPE c LENGTH 30,    " Reference Field
        precfield   TYPE c LENGTH 30,    " Included Table Name
        authorid    TYPE c LENGTH 3,     " Authorization Class
        memoryid    TYPE c LENGTH 20,    " Set/Get Parameter ID
        logflag     TYPE c LENGTH 1,     " Change Documents Indicator
        mask        TYPE c LENGTH 20,    " Template
        masklen     TYPE n LENGTH 4,     " Template Length
        convexit    TYPE c LENGTH 5,     " Conversion Routine
        headlen     TYPE n LENGTH 2,     " Heading Length
        scrlen1     TYPE n LENGTH 2,     " Short Field Label Length
        scrlen2     TYPE n LENGTH 2,     " Medium Field Label Length
        scrlen3     TYPE n LENGTH 2,     " Long Field Label Length
        fieldtext   TYPE c LENGTH 60,    " Short Description
        reptext     TYPE c LENGTH 55,    " Heading
        scrtext_s   TYPE c LENGTH 10,    " Short Field Label
        scrtext_m   TYPE c LENGTH 20,    " Medium Field Label
        scrtext_l   TYPE c LENGTH 40,    " Long Field Label
        keyflag     TYPE c LENGTH 1,     " Key Field Indicator
        lowercase   TYPE c LENGTH 1,     " Lowercase Allowed
        mac         TYPE c LENGTH 1,     " Search Help Attached
        genkey      TYPE c LENGTH 1,     " Flag (X or Blank)
        noforkey    TYPE c LENGTH 1,     " Flag (X or Blank)
        valexi      TYPE c LENGTH 1,     " Fixed Values Exist
        noauthch    TYPE c LENGTH 1,     " Flag (X or Blank)
        sign        TYPE c LENGTH 1,     " Sign Flag
        dynpfld     TYPE c LENGTH 1,     " Field Displayed on Dynpro
        f4availabl  TYPE c LENGTH 1,     " Input Help Available
        comptype    TYPE c LENGTH 1,     " Component Type
        lfieldname  TYPE c LENGTH 132,   " Long Field Name
        ltrflddis   TYPE c LENGTH 1,     " Left-to-Right Write Direction
        bidictrlc   TYPE c LENGTH 1,     " No BIDI Character Filtering
        outputstyle TYPE n LENGTH 2,     " Output Style (Decfloat Types)
        nohistory   TYPE c LENGTH 1,     " Input History Deactivated
        ampmformat  TYPE c LENGTH 1,     " AM/PM Time Format Indicator
      END OF ty_s_dfies_2.
    TYPES ty_t_dfies_2 TYPE STANDARD TABLE OF ty_s_dfies_2 WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_shlp_intdescr,
        issimple         TYPE c LENGTH 1,    " Elementary Search Help Flag
        hotkey           TYPE c LENGTH 1,    " Hot Key
        selmtype         TYPE c LENGTH 1,    " Category of Selection Method
        selmethod        TYPE c LENGTH 30,   " Selection Method Name
        texttab          TYPE c LENGTH 30,   " Text Table Name
        selmexit         TYPE c LENGTH 30,   " Search Help Exit
        dialogtype       TYPE c LENGTH 1,    " Dialog Type
        ddlanguage       TYPE c LENGTH 1,    " Language Key
        ddtext           TYPE c LENGTH 60,   " Short Text
        dialoginfo       TYPE c LENGTH 1,    " Flag: SELFIELDS/LISTFIELDS read
        f4state          TYPE c LENGTH 1,    " Internal Usage Only
        tabname          TYPE c LENGTH 30,   " Table Name
        fieldname        TYPE c LENGTH 30,   " Field Name
        title            TYPE c LENGTH 60,   " Title Text
        history          TYPE c LENGTH 1,    " Deprecated Usage
        handle           TYPE int4,             " Reference Handle (int4)
        autosuggest      TYPE c LENGTH 1,    " Autosuggest Flag
        fuzzy_search     TYPE c LENGTH 1,    " Fuzzy Search Flag
        fuzzy_similarity TYPE p DECIMALS 1 LENGTH 2,  " Accuracy for Fuzzy Search (DEC 2,1)
      END OF ty_shlp_intdescr.

    TYPES:
      BEGIN OF ty_ddshiface,
        shlpfield  TYPE c LENGTH 30,    " Field Name for Pass by Value to F4 Help
        valtabname TYPE c LENGTH 30,    " Structure Name for Input Help Assignment
        valfield   TYPE c LENGTH 132,   " Field for Input Help Assignment
        value      TYPE c LENGTH 132,   " Field Content from Dynpro
        internal   TYPE c LENGTH 1,     " Flag: Internal Representation
        dispfield  TYPE c LENGTH 1,     " Display-Only Field Flag
        f4field    TYPE c LENGTH 1,     " F4 Pressed Flag
        topshlpnam TYPE c LENGTH 30,    " Higher-Level Search Help Name
        topshlpfld TYPE c LENGTH 30,    " Field of Higher-Level Search Help
      END OF ty_ddshiface.

    TYPES:
      BEGIN OF ty_ddshfprop,
        fieldname  TYPE c LENGTH 30,   " Name of Search Help Parameter
        shlpinput  TYPE c LENGTH 1,    " IMPORT Parameter Flag
        shlpoutput TYPE c LENGTH 1,    " EXPORT Parameter Flag
        shlpselpos TYPE n LENGTH 2,    " Position in Dialog Box
        shlplispos TYPE n LENGTH 2,    " Position in Hit List
        shlpseldis TYPE c LENGTH 1,    " Display Field in Dialog Box
        defaultval TYPE c LENGTH 21,   " Default Value
      END OF ty_ddshfprop.

    TYPES:
      BEGIN OF ty_ddshselopt,
        shlpname  TYPE c LENGTH 30,   " Name of Search Help
        shlpfield TYPE c LENGTH 30,   " Name of Search Help Parameter
        sign      TYPE c LENGTH 1,    " Include/Exclude Flag (I/E)
        option    TYPE c LENGTH 2,    " Selection Option (EQ/BT/CP/..)
        low       TYPE c LENGTH 45,   " Low Value for Selection
        high      TYPE c LENGTH 45,   " High Value for Selection
      END OF ty_ddshselopt.

    TYPES:
      BEGIN OF ty_ddshtextsearch_field,
        fieldname TYPE c LENGTH 30,    " Name of a searchable field (frei gewählt, da nicht spezifiziert)
      END OF ty_ddshtextsearch_field.

    TYPES: tt_ddshtextsearch_fields TYPE STANDARD TABLE OF ty_ddshtextsearch_field WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_ddshtextsearch,
        request TYPE c LENGTH 60,               " Text Search Request
        fields  TYPE tt_ddshtextsearch_fields,  " Fields eligible for text search
      END OF ty_ddshtextsearch.

    TYPES:
      BEGIN OF ty_shlp_descr,
        shlpname   TYPE c LENGTH 30,  " Name of a Search Help
        shlptype   TYPE c LENGTH 2,   " Type of an input help (fixed values)
        intdescr   TYPE ty_shlp_intdescr,  " Placeholder for Internal Info of Search Help
        interface  TYPE STANDARD TABLE OF ty_ddshiface WITH EMPTY KEY,  " Placeholder for Interface of Search Help
        fielddescr TYPE STANDARD TABLE OF ty_s_dfies_2 WITH EMPTY KEY,
        fieldprop  TYPE STANDARD TABLE OF ty_ddshfprop WITH EMPTY KEY,
        selopt     TYPE STANDARD TABLE OF ty_ddshselopt WITH EMPTY KEY,
        textsearch TYPE ty_ddshtextsearch,
      END OF ty_shlp_descr.


    CLASS-METHODS bus_tr_read
      RETURNING
        VALUE(mt_data) TYPE ty_t_data.

    CLASS-METHODS bus_tr_add
      IMPORTING
        ir_data      TYPE REF TO data
        iv_tabname   TYPE string
        is_transport TYPE ty_s_transport
      EXCEPTIONS
        ob_check_obj_error.

    CLASS-METHODS bus_search_help_read
      CHANGING
        ms_shlp        TYPE ty_shlp_descr
        mv_fname       TYPE string
        mv_table       TYPE string
        mr_data        TYPE REF TO data
        mt_result_desc TYPE ty_t_dfies_2
        mv_shlpfield   TYPE string
        mt_data        TYPE REF TO data
        ms_data_row    TYPE REF TO data.

  PROTECTED SECTION.

    CLASS-METHODS _set_e071k
      IMPORTING
        ir_data       TYPE REF TO data
        iv_tabname    TYPE string
        is_transport  TYPE ty_s_transport
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS _set_e071
      IMPORTING
        iv_tabname    TYPE string
        is_transport  TYPE ty_s_transport
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS _get_e071k_tabkey
      IMPORTING
        !line            TYPE any
        dfies            TYPE z2ui5_cl_util=>ty_t_dfies
      RETURNING
        VALUE(rv_tabkey) TYPE trobj_name.

    CLASS-METHODS _read_e070
      CHANGING
        mt_data TYPE ty_t_data.

    CLASS-METHODS set_mandt
      IMPORTING
        ir_data TYPE REF TO data.

  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_cl_util_abap_new IMPLEMENTATION.


  METHOD bus_search_help_read.

    DATA lt_shlp       TYPE shlp_desct.
    DATA lt_result_tab TYPE TABLE OF string.
    DATA ls_comp       TYPE abap_componentdescr.
    DATA lt_comps      TYPE abap_component_tab.
    DATA lo_datadescr  TYPE REF TO cl_abap_datadescr.
    DATA lr_line       TYPE REF TO data.

*    data ls_shlp type shlp_descr.

    data lr_shlp type ref to data.
    create data lr_shlp type ('SHLP_DESCR').
    assign lr_shlp->* to field-symbol(<shlp>).

    data lv_tabname type c length 30.
    data lv_fieldname type c length 30.
    lv_tabname = mv_table.
    lv_fieldname = mv_fname.

    IF ms_shlp IS INITIAL.
      " Suchhilfe lesen
      data(lv_fm) = 'F4IF_DETERMINE_SEARCHHELP'.
      CALL FUNCTION lv_fm
        EXPORTING
          tabname           = lv_tabname
          fieldname         = lv_fieldname
        IMPORTING
          shlp              = <shlp>
        EXCEPTIONS
          field_not_found   = 1
          no_help_for_field = 2
          inconsistent_help = 3
          OTHERS            = 4.
      IF sy-subrc <> 0.
        " FEHLER
      ENDIF.
    ms_shlp = CORRESPONDING #( <shlp> ).

      IF ms_shlp-intdescr-issimple = abap_false.
      lv_fm = 'F4IF_EXPAND_SEARCHHELP'.
        CALL FUNCTION lv_fm
          EXPORTING
            shlp_top = ms_shlp
          IMPORTING
            shlp_tab = lt_shlp.

        DATA(ls_row) = VALUE #( lt_shlp[ 1 ] OPTIONAL ).
        ms_shlp = CORRESPONDING #( ls_row ).
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

    create data lr_shlp type ('SHLP_DESCR').
    assign lr_shlp->* to <shlp>.
    <shlp> = CORRESPONDING #( ms_shlp ).

    lv_fm = 'F4IF_SELECT_VALUES'.
    CALL FUNCTION lv_fm
      EXPORTING
        shlp           = <shlp>
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

            TRY.
                " Sting table will crash if value length <> outputlen
                <line_content> = result_line+result_desc-offset.
              CATCH cx_root.
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

*    set_row_id( ).

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



  METHOD _get_e071k_tabkey.

    DATA lv_type       TYPE c LENGTH 1.
    DATA lv_tabkey     TYPE c LENGTH 999.
    DATA lv_tabkey_len TYPE i.
    DATA lv_field_len  TYPE i.
    DATA lv_offset     TYPE i.

    LOOP AT dfies INTO DATA(s_dfies) WHERE keyflag = abap_true.

      ASSIGN COMPONENT s_dfies-fieldname OF STRUCTURE line TO FIELD-SYMBOL(<value>).
      IF <value> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      lv_type = cl_abap_typedescr=>describe_by_data( <value> )->type_kind.

      IF lv_type NA 'CDNT'.
        lv_tabkey+lv_tabkey_len = '*'.
        rv_tabkey = lv_tabkey.
        RETURN.
      ELSE.
        lv_field_len = cl_abap_typedescr=>describe_by_data( <value> )->length / cl_abap_char_utilities=>charsize.
      ENDIF.

      lv_field_len = cl_abap_typedescr=>describe_by_data( <value> )->length / cl_abap_char_utilities=>charsize.
      lv_tabkey+lv_tabkey_len(lv_field_len) = <value>.
      lv_tabkey_len = lv_tabkey_len + lv_field_len.

    ENDLOOP.

    IF lv_tabkey_len > 119.

      IF lv_tabkey CS '_'.
        lv_offset = sy-fdpos.
        lv_tabkey+lv_offset = '*'.
      ELSE.
        lv_tabkey+119 = '*'.
      ENDIF.

    ENDIF.
    rv_tabkey = lv_tabkey.
  ENDMETHOD.

  METHOD bus_tr_add.


    IF z2ui5_cl_util=>context_check_abap_cloud( ).







    ELSE.


      FIELD-SYMBOLS <e071>    TYPE any.
      FIELD-SYMBOLS <t_e071k> TYPE STANDARD TABLE.
      FIELD-SYMBOLS <t_e071>  TYPE STANDARD TABLE.

      " We need to set the MANDT is necessary
      set_mandt( ir_data ).

      DATA(r_e071k) = _set_e071k( ir_data      = ir_data
                                 iv_tabname   = iv_tabname
                                 is_transport = is_transport ).
      ASSIGN r_e071k->* TO <e071>.
      IF <e071> IS INITIAL.
        RETURN.
      ENDIF.

      DATA(r_e071) = _set_e071( iv_tabname   = iv_tabname
                               is_transport = is_transport ).

      ASSIGN r_e071k->* TO <t_e071k>.
      ASSIGN r_e071->* TO <t_e071>.

      DATA(fb1) = 'TR_APPEND_TO_COMM_OBJS_KEYS'.

      CALL FUNCTION fb1
        EXPORTING
          wi_trkorr = is_transport-transport
          iv_dialog = abap_false
        TABLES
          wt_e071   = <t_e071>
          wt_e071k  = <t_e071k>
        EXCEPTIONS
          OTHERS    = 1.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE z2ui5_cx_util_error.
      ENDIF.

      DATA(fb2) = 'TR_SORT_AND_COMPRESS_COMM'.

      CALL FUNCTION fb2
        EXPORTING
          iv_trkorr = is_transport-task
        EXCEPTIONS
          OTHERS    = 1.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE z2ui5_cx_util_error.
      ELSE.
        COMMIT WORK AND WAIT.
      ENDIF.

    ENDIF.

  ENDMETHOD.



  METHOD _set_e071k.

    DATA t_e071k TYPE REF TO data.
    DATA s_e071k TYPE REF TO data.

    FIELD-SYMBOLS <t_e071k> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <s_e071k> TYPE any.
    FIELD-SYMBOLS <value>   TYPE any.
    FIELD-SYMBOLS <tab>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line>    TYPE any.

    DATA(t_comp) = z2ui5_cl_util=>rtti_get_t_attri_by_table_name( 'E071K' ).

    TRY.

        DATA(struct_desc) = cl_abap_structdescr=>create( t_comp ).

        DATA(table_desc) = cl_abap_tabledescr=>create( p_line_type  = struct_desc
                                                       p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA t_e071k TYPE HANDLE table_desc.
        CREATE DATA s_e071k TYPE HANDLE struct_desc.

        ASSIGN t_e071k->* TO <t_e071k>.
        ASSIGN s_e071k->* TO <s_e071k>.

      CATCH cx_root.

    ENDTRY.

    DATA(dfies) = z2ui5_cl_util=>rtti_get_t_dfies_by_table_name( iv_tabname ).

*   is_transport-transport = assign_value( component = 'TRKORR'
*                                          structure = <s_e071k> ).                                         )

    ASSIGN COMPONENT 'TRKORR' OF STRUCTURE <s_e071k> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = is_transport-task.
    ENDIF.
    UNASSIGN <value>.
    ASSIGN COMPONENT 'PGMID' OF STRUCTURE <s_e071k> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = 'R3TR'.
    ENDIF.
    UNASSIGN <value>.
    ASSIGN COMPONENT 'MASTERTYPE' OF STRUCTURE <s_e071k> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = 'TABU'.
    ENDIF.
    UNASSIGN <value>.
    ASSIGN COMPONENT 'OBJECT' OF STRUCTURE <s_e071k> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = 'TABU'.
    ENDIF.
    UNASSIGN <value>.
    ASSIGN COMPONENT 'MASTERNAME' OF STRUCTURE <s_e071k> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = iv_tabname.
    ENDIF.
    UNASSIGN <value>.
    ASSIGN COMPONENT 'OBJNAME' OF STRUCTURE <s_e071k> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = iv_tabname.
    ENDIF.
    UNASSIGN <value>.

    ASSIGN ir_data->* TO <tab>.

*    IF <tab> IS INITIAL.
*      RETURN.
*    ENDIF.

    LOOP AT <tab> ASSIGNING <line>.

      ASSIGN COMPONENT 'TABKEY' OF STRUCTURE <s_e071k> TO <value>.
      IF <value> IS NOT ASSIGNED.
        RETURN.
      ELSE.
        <value> = _get_e071k_tabkey( dfies = dfies
                                    line  = <line> ).
      ENDIF.

      APPEND <s_e071k> TO <t_e071k>.

    ENDLOOP.

    result = t_e071k.

  ENDMETHOD.

  METHOD _set_e071.

    DATA t_e071 TYPE REF TO data.
    DATA s_e071 TYPE REF TO data.

    FIELD-SYMBOLS <t_e071> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <s_e071> TYPE any.
    FIELD-SYMBOLS <value>  TYPE any.

    DATA(t_comp) = z2ui5_cl_util=>rtti_get_t_attri_by_table_name( 'E071' ).

    TRY.

        DATA(struct_desc_new) = cl_abap_structdescr=>create( t_comp ).

        DATA(table_desc_new) = cl_abap_tabledescr=>create( p_line_type  = struct_desc_new
                                                           p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA t_e071 TYPE HANDLE table_desc_new.
        CREATE DATA s_e071 TYPE HANDLE struct_desc_new.

        ASSIGN t_e071->* TO <t_e071>.
        ASSIGN s_e071->* TO <s_e071>.

      CATCH cx_root.

    ENDTRY.

    ASSIGN COMPONENT 'TRKORR' OF STRUCTURE <s_e071> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = is_transport-task.
    ENDIF.
    UNASSIGN <value>.
    ASSIGN COMPONENT 'PGMID' OF STRUCTURE <s_e071> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = 'R3TR'.
    ENDIF.
    UNASSIGN <value>.
    ASSIGN COMPONENT 'OBJECT' OF STRUCTURE <s_e071> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = 'TABU'.
    ENDIF.
    UNASSIGN <value>.
    ASSIGN COMPONENT 'OBJ_NAME' OF STRUCTURE <s_e071> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = iv_tabname.
    ENDIF.
    UNASSIGN <value>.
    ASSIGN COMPONENT 'OBJFUNC' OF STRUCTURE <s_e071> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = 'K'.
    ENDIF.
    UNASSIGN <value>.

    APPEND <s_e071> TO <t_e071>.

    result = t_e071.

  ENDMETHOD.

  METHOD _read_e070.

    DATA lo_tab  TYPE REF TO data.
    DATA lo_line TYPE REF TO data.
    DATA ls_data TYPE ty_s_transport.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line>  TYPE any.
    FIELD-SYMBOLS <value> TYPE any.

    DATA(table_name) = 'E070'.

    TRY.
        DATA(t_comp) = z2ui5_cl_util=>rtti_get_t_attri_by_table_name( table_name ).

        DATA(new_struct_desc) = cl_abap_structdescr=>create( t_comp ).

        DATA(new_table_desc) = cl_abap_tabledescr=>create( p_line_type  = new_struct_desc
                                                           p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA lo_tab TYPE HANDLE new_table_desc.
        CREATE DATA lo_line TYPE HANDLE new_struct_desc.

        ASSIGN lo_tab->* TO <table>.
        ASSIGN lo_line->* TO <line>.

        DATA(where) =
        |( TRFUNCTION EQ 'Q' ) AND ( TRSTATUS EQ 'D' ) AND ( KORRDEV EQ 'CUST' ) AND ( AS4USER EQ '{ sy-uname }' )|.

        SELECT trkorr,
               trfunction,
               trstatus,
               tarsystem,
               korrdev,
               as4user,
               as4date,
               as4time,
               strkorr
          FROM (table_name)
          WHERE (where)
          INTO TABLE @<table>.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
      CATCH cx_root.
    ENDTRY.

    LOOP AT <table> INTO <line>.

      ASSIGN COMPONENT 'TRKORR' OF STRUCTURE <line> TO <value>.
      IF <value> IS NOT ASSIGNED.
        CONTINUE.
      ELSE.
        ls_data-transport = <value>.
      ENDIF.

      UNASSIGN <value>.

      ASSIGN COMPONENT 'STRKORR' OF STRUCTURE <line> TO <value>.
      IF <value> IS NOT ASSIGNED.
        CONTINUE.
      ELSE.
        ls_data-task = <value>.
      ENDIF.

      UNASSIGN <value>.

      APPEND ls_data TO mt_data.

    ENDLOOP.

  ENDMETHOD.

  METHOD bus_tr_read.


    IF z2ui5_cl_util=>context_check_abap_cloud( ).

*          data(lo_current_user) = xco_cp=>sy->user( ).
*
*    DATA(lo_kind_filter) = xco_cp_transport=>filter->kind( xco_cp_transport=>kind->task ).
*    DATA(lo_owner_filter) = xco_cp_transport=>filter->owner( xco_cp_abap_sql=>constraint->equal( lo_current_user->name ) ).
*    DATA(lo_status_filter) = xco_cp_transport=>filter->status( xco_cp_transport=>status->modifiable ).
*    DATA(lo_type_filter) = xco_cp_transport=>filter->type( io_type = xco_cp_transport=>type->customizing_task ).
*    DATA(lt_transports) = xco_cp_cts=>transports->where( VALUE #( ( lo_kind_filter )
*                                                                  ( lo_owner_filter )
*                                                                  ( lo_status_filter )
*                                                                  ( lo_type_filter ) )
*    )->resolve( xco_cp_transport=>resolution->request ).
*
*    LOOP AT lt_transports INTO DATA(lo_transport).
*      DATA(lo_transport_request) = lo_transport->get_request( ).
*
*      DATA(prop) = lo_transport_request->properties( )->get( ).
*
*      DATA(tasks) = lo_transport_request->get_tasks( ).
*
*      LOOP AT tasks INTO DATA(task).
*
*        IF lo_current_user->name = task->properties( )->get_owner( )->name.
*
*          DATA(data) = VALUE ty_s_data( short_description = prop-short_description
*                                        transport         = lo_transport_request->value
*                                        task              = task->value ).
*          APPEND data TO mt_data.
*
*        ENDIF.
*
*      ENDLOOP.
*
*    ENDLOOP.

    ELSE.

      DATA lo_tab  TYPE REF TO data.
      DATA lo_line TYPE REF TO data.

      FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
      FIELD-SYMBOLS <line>  TYPE any.
      FIELD-SYMBOLS <value> TYPE any.

      _read_e070( CHANGING mt_data = mt_data ).

      DATA(table_name) = 'E07T'.

      TRY.
          DATA(t_comp) = z2ui5_cl_util=>rtti_get_t_attri_by_table_name( table_name ).

          DATA(new_struct_desc) = cl_abap_structdescr=>create( t_comp ).

          DATA(new_table_desc) = cl_abap_tabledescr=>create( p_line_type  = new_struct_desc
                                                             p_table_kind = cl_abap_tabledescr=>tablekind_std ).

          CREATE DATA lo_tab TYPE HANDLE new_table_desc.
          CREATE DATA lo_line TYPE HANDLE new_struct_desc.

          ASSIGN lo_tab->* TO <table>.
          ASSIGN lo_line->* TO <line>.

          DATA(index) = 0.

          LOOP AT mt_data INTO DATA(line).
            index = index + 1.
            IF index = 1.
              DATA(where) = |TRKORR EQ '{ line-task }'|.
            ELSE.
              where = |{ where }OR TRKORR EQ '{ line-task }'|.
            ENDIF.
            where = |( { where } )|.
          ENDLOOP.

          SELECT trkorr,
                 langu,
                 as4text
            FROM (table_name)
            WHERE (where)
            INTO TABLE @<table>.
          IF sy-subrc <> 0.
            RETURN.
          ENDIF.

        CATCH cx_root.
      ENDTRY.

      LOOP AT <table> INTO <line>.

        ASSIGN COMPONENT 'TRKORR' OF STRUCTURE <line> TO <value>.
        IF <value> IS NOT ASSIGNED.
          CONTINUE.
        ELSE.

          READ TABLE mt_data REFERENCE INTO DATA(data) WITH KEY task = <value>.
          IF sy-subrc = 0.

            ASSIGN COMPONENT 'AS4TEXT' OF STRUCTURE <line> TO <value>.
            IF <value> IS NOT ASSIGNED.
              CONTINUE.
            ELSE.

              data->short_description = <value>.

            ENDIF.

          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD set_mandt.

    FIELD-SYMBOLS <tab>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE any.

    ASSIGN ir_data->* TO <tab>.

    LOOP AT <tab> ASSIGNING <line>.

      ASSIGN COMPONENT `MANDT` OF STRUCTURE <line> TO FIELD-SYMBOL(<row>).
      IF <row> IS ASSIGNED.

        TRY.
            <row> = sy-mandt.
          CATCH cx_root.
        ENDTRY.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.


ENDCLASS.
