CLASS z2ui5_cl_popup_context DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF cs_ui5_msg_type,
        e TYPE string VALUE `Error` ##NO_TEXT,
        s TYPE string VALUE `Success` ##NO_TEXT,
        w TYPE string VALUE `Warning` ##NO_TEXT,
        i TYPE string VALUE `Information` ##NO_TEXT,
      END OF cs_ui5_msg_type.

    " Environment-abstracted character/format constants. Callers must read
    " these from z2ui5_cl_util instead of referencing cl_abap_char_utilities /
    " cl_abap_format directly, so the dependency on those SAP standard classes
    " lives in exactly one place (this class' class_constructor) and can be
    " ported once for non-ABAP runtimes (e.g. transpiled JS).
    CLASS-DATA cv_char_util_newline        TYPE c LENGTH 1 READ-ONLY.

    CLASS-DATA cv_char_util_cr_lf          TYPE c LENGTH 2 READ-ONLY.

    CLASS-DATA cv_char_util_horizontal_tab TYPE c LENGTH 1 READ-ONLY.

    CLASS-DATA cv_char_util_charsize       TYPE i          READ-ONLY.

    CLASS-DATA cv_format_e_xml_attr             TYPE i          READ-ONLY.

    " RTTI type-kind / kind / visibility constants, so callers can branch on
    " stored type_kind/kind fields without referencing cl_abap_typedescr /
    " cl_abap_objectdescr directly.
    CLASS-DATA cv_typedescr_typekind_table   TYPE c LENGTH 1 READ-ONLY.

    CLASS-DATA cv_typedescr_typekind_dref    TYPE c LENGTH 1 READ-ONLY.

    CLASS-DATA cv_typedescr_typekind_oref    TYPE c LENGTH 1 READ-ONLY.

    CLASS-DATA cv_typedescr_typekind_struct1 TYPE c LENGTH 1 READ-ONLY.

    CLASS-DATA cv_typedescr_typekind_struct2 TYPE c LENGTH 1 READ-ONLY.

    " the three elementary kinds whose JSON form is not their ABAP form
    " (ISO date/time strings, ISO timestamps) - what a caller converting a
    " scalar between the two representations has to branch on
    CLASS-DATA cv_typedescr_typekind_date    TYPE c LENGTH 1 READ-ONLY.

    CLASS-DATA cv_typedescr_typekind_time    TYPE c LENGTH 1 READ-ONLY.

    CLASS-DATA cv_typedescr_typekind_packed  TYPE c LENGTH 1 READ-ONLY.

    CLASS-DATA cv_typedescr_kind_struct      TYPE c LENGTH 1 READ-ONLY.

    CLASS-DATA cv_typedescr_kind_elem        TYPE c LENGTH 1 READ-ONLY.

    CLASS-DATA cv_typedescr_kind_ref         TYPE c LENGTH 1 READ-ONLY.

    CLASS-DATA cv_objectdescr_public         TYPE c LENGTH 1 READ-ONLY.

    CLASS-METHODS class_constructor.

    TYPES:
      BEGIN OF ty_s_name_value,
        n TYPE string,
        v TYPE string,
      END OF ty_s_name_value.

    TYPES ty_t_name_value TYPE STANDARD TABLE OF ty_s_name_value WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_token,
        key      TYPE string,
        text     TYPE string,
        visible  TYPE abap_bool,
        selkz    TYPE abap_bool,
        editable TYPE abap_bool,
      END OF ty_s_token.

    TYPES ty_t_token TYPE STANDARD TABLE OF ty_s_token WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_range,
        sign   TYPE c LENGTH 1,
        option TYPE c LENGTH 2,
        low    TYPE string,
        high   TYPE string,
      END OF ty_s_range.

    TYPES ty_t_range TYPE STANDARD TABLE OF ty_s_range WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_filter_multi,
        name            TYPE string,
        t_range         TYPE ty_t_range,
        t_token         TYPE ty_t_token,
        t_token_added   TYPE ty_t_token,
        t_token_removed TYPE ty_t_token,
      END OF ty_s_filter_multi.

    TYPES ty_t_filter_multi TYPE STANDARD TABLE OF ty_s_filter_multi WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_msg,
        text       TYPE string,
        id         TYPE string,
        no         TYPE string,
        type       TYPE string,
        v1         TYPE string,
        v2         TYPE string,
        v3         TYPE string,
        v4         TYPE string,
        timestampl TYPE timestampl,
        t_meta     TYPE ty_t_name_value,
      END OF ty_s_msg,
      ty_t_msg TYPE STANDARD TABLE OF ty_s_msg WITH DEFAULT KEY.

    CLASS-METHODS ui5_get_msg_type
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS msg_get_t
      IMPORTING
        !val          TYPE any
        !val2         TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_t_msg.

    CLASS-METHODS rtti_get_data_element_text_l
      IMPORTING
        !val          TYPE any
      RETURNING
        VALUE(result) TYPE string.

    " Extracts the DDIC data element name (`\TYPE=...` part of the absolute
    " name) from a component's elementary type, so callers do not reference
    " cl_abap_elemdescr directly.
    CLASS-METHODS rtti_get_ddic_type_name
      IMPORTING
        type          TYPE REF TO cl_abap_datadescr
      RETURNING
        VALUE(result) TYPE string.

    TYPES:
      BEGIN OF ty_s_sel_tab_type,
        tabledescr       TYPE REF TO cl_abap_tabledescr,
        check_table_line TYPE abap_bool,
      END OF ty_s_sel_tab_type.

    " Builds a working table type from the line type of the source table. If
    " the source line is elementary it is wrapped into a `TAB_LINE` component
    " (check_table_line reports whether that happened). By default no extra
    " column is added; when add_sel_field = abap_true an additional boolean
    " flag column named sel_field_name (default `ZZSELKZ`) is inserted.
    " Keeps the dynamic RTTI type construction (describe_by_data / create) in
    " one place so it can be ported once for non-ABAP runtimes.
    " `ir_tab` keeps its Hungarian name against the `val` convention used by
    " the rest of this class: abap2UI5's frozen src/99 package passes it as a
    " NAMED argument and cannot be edited, so renaming it there is blocked -
    " and a rename only in this catalog would show up as drift on every sync.
    " Rename it once that consumer drops the call.
    CLASS-METHODS rtti_create_sel_tab_type
      IMPORTING
        ir_tab         TYPE REF TO data
        add_sel_field  TYPE abap_bool DEFAULT abap_false
        sel_field_name TYPE clike     DEFAULT `ZZSELKZ`
      RETURNING
        VALUE(result)  TYPE ty_s_sel_tab_type.

    " depth guards the mutual recursion between these two (an include whose
    " expansion reaches itself would otherwise be an unbounded stack).
    " Optional and last in the list, so every existing caller is unaffected
    CLASS-METHODS rtti_get_t_attri_by_include
      IMPORTING
        !type         TYPE REF TO cl_abap_datadescr
        depth         TYPE i DEFAULT 0
      RETURNING
        VALUE(result) TYPE abap_component_tab.

    CLASS-METHODS expand_components
      IMPORTING
        val           TYPE abap_component_tab
        depth         TYPE i DEFAULT 0
      RETURNING
        VALUE(result) TYPE abap_component_tab.

    CLASS-METHODS filter_itab
      IMPORTING
        filter TYPE ty_t_filter_multi
      CHANGING
        val     TYPE STANDARD TABLE.

    CLASS-METHODS filter_get_multi_by_data
      IMPORTING
        val           TYPE data
      RETURNING
        VALUE(result) TYPE ty_t_filter_multi.

    CLASS-METHODS url_param_get
      IMPORTING
        val           TYPE clike
        url           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS rtti_get_classname_by_ref
      IMPORTING
        val           TYPE REF TO object
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS boolean_check_by_data
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS boolean_abap_2_json
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS time_get_timestampl
      RETURNING
        VALUE(result) TYPE timestampl.

    CLASS-METHODS c_trim
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS c_trim_lower
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS url_param_get_tab
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE ty_t_name_value.

    CLASS-METHODS rtti_get_t_attri_by_oref
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_attrdescr_tab.

    CLASS-METHODS rtti_get_t_attri_by_any
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE cl_abap_structdescr=>component_table.

    CLASS-METHODS rtti_get_t_attri_by_table_name
      IMPORTING
        table_name    TYPE any
      RETURNING
        VALUE(result) TYPE cl_abap_structdescr=>component_table.

    CLASS-METHODS rtti_get_type_kind
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS rtti_check_ref_data
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS boolean_check_by_name
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS filter_get_range_t_by_token_t
      IMPORTING
        val           TYPE ty_t_token
      RETURNING
        VALUE(result) TYPE ty_t_range.

    CLASS-METHODS filter_get_range_by_token
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE ty_s_range.

    CLASS-METHODS filter_get_token_t_by_range_t
      IMPORTING
        val           TYPE ANY TABLE
      RETURNING
        VALUE(result) TYPE ty_t_token ##NEEDED.

    CLASS-METHODS filter_get_token_range_mapping
      RETURNING
        VALUE(result) TYPE ty_t_name_value.

    CLASS-METHODS itab_corresponding
      IMPORTING
        val  TYPE STANDARD TABLE
      CHANGING
        !tab TYPE STANDARD TABLE.

    CLASS-METHODS itab_filter_by_val
      IMPORTING
        val         TYPE clike
        fields      TYPE string_table OPTIONAL
        ignore_case TYPE abap_bool DEFAULT abap_false
      CHANGING
        !tab        TYPE STANDARD TABLE.

    CLASS-METHODS itab_get_by_struc
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE ty_t_name_value.

    CLASS-METHODS itab_filter_by_t_range
      IMPORTING
        val  TYPE ty_t_filter_multi
      CHANGING
        !tab TYPE STANDARD TABLE.

    CLASS-METHODS conv_copy_ref_data
      IMPORTING
        !from         TYPE any
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS conv_get_xstring_by_data_uri
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE xstring.

    TYPES:
      BEGIN OF ty_s_data_uri,
        mime_type TYPE string,
        content   TYPE xstring,
      END OF ty_s_data_uri.

    "! A data URI (RFC 2397, data:[mime][;charset=...][;base64],payload) as
    "! MIME type and content: the payload base64-decoded when the header
    "! says `;base64`, otherwise percent-decoded to its bytes (text between
    "! the escapes as UTF-8). The MIME type is lower case; when the URI
    "! names none, or only application/octet-stream, it is taken from
    "! file_name (file_get_mimetype) if one is given. A bare base64 payload
    "! without the `data:...,` prefix is decoded as base64.
    CLASS-METHODS conv_get_file_by_data_uri
      IMPORTING
        val           TYPE clike
        file_name     TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_s_data_uri.

    CLASS-METHODS rtti_check_clike
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS rtti_check_table
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS rtti_check_structure
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    TYPES:
      BEGIN OF ty_s_data_element_text,
        header TYPE string,
        short  TYPE string,
        medium TYPE string,
        long   TYPE string,
      END OF ty_s_data_element_text.

    CLASS-METHODS check_abap_cloud
      RETURNING
        VALUE(result) TYPE abap_bool.

    " the system fields sy_get hands out, in the types SYST gives them
    TYPES:
      BEGIN OF ty_s_sy,
        datum TYPE d,
        uzeit TYPE t,
        uname TYPE c LENGTH 12,
        sysid TYPE c LENGTH 8,
        saprl TYPE c LENGTH 4,
      END OF ty_s_sy.

    " THE one place that reads a system field for its VALUE - date, time,
    " user, system id, release. Every other class asks here
    " (sy_get( )-uname), for two reasons: ABAP for Cloud Development reports
    " the old system fields on every statement that names one ("The old
    " variant of SY-DATUM should not be used ..."), so one reader is one
    " finding per field instead of one per use; and the day one of them
    " has to come from another API, it changes here and nowhere else.
    " Not meant: sy-subrc, sy-tabix and sy-index - those are the result of
    " the statement before them and have to be read where it stands
    CLASS-METHODS sy_get
      RETURNING
        VALUE(result) TYPE ty_s_sy.

    CLASS-METHODS uuid_get_c32
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS rtti_get_data_element_texts
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE ty_s_data_element_text.

    CLASS-METHODS conv_decode_x_base64
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE xstring.

    CLASS-METHODS conv_encode_x_base64
      IMPORTING
        val           TYPE xstring
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS conv_get_string_by_xstring
      IMPORTING
        val           TYPE xstring
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS conv_get_xstring_by_string
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE xstring.

    " abap_true for values that can be rendered into a text without a
    " conversion dump - the elementary types. Anything structured (struct,
    " table, reference) must be excluded before a generic `|{ val }|`.
    CLASS-METHODS rtti_check_printable
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS rtti_get_table_desrc
      IMPORTING
        tabname       TYPE clike
        langu         TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE string ##NEEDED.

    CLASS-METHODS msg_map
      IMPORTING
        name          TYPE clike
        val           TYPE data
        msg           TYPE ty_s_msg
      RETURNING
        VALUE(result) TYPE ty_s_msg.

    TYPES: BEGIN OF ty_usr01,
             mandt         TYPE c LENGTH 3,     " Client
             bname         TYPE c LENGTH 12,    " User Name
             stcod         TYPE c LENGTH 20,    " Start Menu (old)
             spld          TYPE c LENGTH 4,     " Output Device
             splg          TYPE c LENGTH 1,     " Print Parameter 1
             spdb          TYPE c LENGTH 1,     " Print Parameter 2
             spda          TYPE c LENGTH 1,     " Print Parameter 3
             datfm         TYPE c LENGTH 1,     " Date Format
             dcpfm         TYPE c LENGTH 1,     " Decimal Format
             hdest         TYPE c LENGTH 8,     " Host Destination
             hmand         TYPE c LENGTH 3,     " Default Host Client
             hname         TYPE c LENGTH 12,    " Default Host Username
             menon         TYPE c LENGTH 1,     " Automatic Start
             menue         TYPE c LENGTH 20,    " Menu Name
             strtt         TYPE c LENGTH 20,    " Start Menu (again)
             langu         TYPE c LENGTH 1,     " Logon Language
             cattkennz     TYPE c LENGTH 1,     " CATT Test Status
             timefm        TYPE c LENGTH 1,     " Time Format (12h/24h)
             ianatzonecode TYPE n LENGTH 4,     " IANA Timezone Code
           END OF ty_usr01.

    TYPES:
      BEGIN OF ty_s_dfies,
        tabname     TYPE c LENGTH 30,
        fieldname   TYPE c LENGTH 30,
        langu       TYPE string,
        position    TYPE n LENGTH 4,
        offset      TYPE n LENGTH 6,
        domname     TYPE c LENGTH 30,
        rollname    TYPE c LENGTH 30,
        checktable  TYPE c LENGTH 30,
        leng        TYPE n LENGTH 6,
        intlen      TYPE n LENGTH 6,
        outputlen   TYPE n LENGTH 6,
        decimals    TYPE n LENGTH 6,
        datatype    TYPE c LENGTH 4,
        inttype     TYPE c LENGTH 1,
        reftable    TYPE c LENGTH 30,
        reffield    TYPE c LENGTH 30,
        precfield   TYPE c LENGTH 30,
        authorid    TYPE c LENGTH 3,
        memoryid    TYPE c LENGTH 20,
        logflag     TYPE c LENGTH 1,
        mask        TYPE c LENGTH 20,
        masklen     TYPE n LENGTH 4,
        convexit    TYPE c LENGTH 5,
        headlen     TYPE n LENGTH 2,
        scrlen1     TYPE n LENGTH 2,
        scrlen2     TYPE n LENGTH 2,
        scrlen3     TYPE n LENGTH 2,
        fieldtext   TYPE c LENGTH 60,
        reptext     TYPE c LENGTH 55,
        scrtext_s   TYPE c LENGTH 10,
        scrtext_m   TYPE c LENGTH 20,
        scrtext_l   TYPE c LENGTH 40,
        keyflag     TYPE c LENGTH 1,
        lowercase   TYPE c LENGTH 1,
        mac         TYPE c LENGTH 1,
        genkey      TYPE c LENGTH 1,
        noforkey    TYPE c LENGTH 1,
        valexi      TYPE c LENGTH 1,
        noauthch    TYPE c LENGTH 1,
        sign        TYPE c LENGTH 1,
        dynpfld     TYPE c LENGTH 1,
        f4availabl  TYPE c LENGTH 1,
        comptype    TYPE c LENGTH 1,
        lfieldname  TYPE c LENGTH 132,
        ltrflddis   TYPE c LENGTH 1,
        bidictrlc   TYPE c LENGTH 1,
        outputstyle TYPE n LENGTH 2,
        nohistory   TYPE c LENGTH 1,
        ampmformat  TYPE c LENGTH 1,
      END OF ty_s_dfies,
      ty_t_dfies TYPE STANDARD TABLE OF ty_s_dfies WITH DEFAULT KEY.

    CLASS-METHODS rtti_get_t_dfies_by_table_name
      IMPORTING
        table_name    TYPE string
      RETURNING
        VALUE(result) TYPE ty_t_dfies.

    CLASS-METHODS tab_get_where_by_dfies
      IMPORTING
        mv_check_tab_field TYPE string
        ms_data_row        TYPE REF TO data
        it_dfies           TYPE ty_t_dfies
      RETURNING
        VALUE(result)      TYPE string.

    TYPES trobj_name     TYPE c LENGTH 120.

    TYPES sxco_transport TYPE c LENGTH 20.

    TYPES:
      BEGIN OF ty_s_transport,
        short_description TYPE string,
        transport         TYPE sxco_transport,
        task              TYPE sxco_transport,
        selkz             TYPE abap_bool,
        locl              TYPE abap_bool,
      END OF ty_s_transport.

    TYPES ty_t_data TYPE STANDARD TABLE OF ty_s_transport WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_dfies_2,
        tabname     TYPE c LENGTH 30,  " Table Name
        fieldname   TYPE c LENGTH 30,  " Field Name
        langu       TYPE c LENGTH 1,   " Language Key
        position    TYPE n LENGTH 4,   " Position of the field in the table
        offset      TYPE n LENGTH 6,   " Offset of a field
        domname     TYPE c LENGTH 30,  " Domain name
        rollname    TYPE c LENGTH 30,  " Data element (semantic domain)
        checktable  TYPE c LENGTH 30,  " Check Table
        leng        TYPE n LENGTH 6,   " Length (Characters)
        intlen      TYPE n LENGTH 6,   " Internal Length (Bytes)
        outputlen   TYPE n LENGTH 6,   " Output Length
        decimals    TYPE n LENGTH 6,   " Number of Decimal Places
        datatype    TYPE c LENGTH 4,   " Dynpro Data Type
        inttype     TYPE c LENGTH 1,   " ABAP Data Type (C,D,N,...)
        reftable    TYPE c LENGTH 30,  " Reference Table
        reffield    TYPE c LENGTH 30,  " Reference Field
        precfield   TYPE c LENGTH 30,  " Included Table Name
        authorid    TYPE c LENGTH 3,   " Authorization Class
        memoryid    TYPE c LENGTH 20,  " Set/Get Parameter ID
        logflag     TYPE c LENGTH 1,   " Change Documents Indicator
        mask        TYPE c LENGTH 20,  " Template
        masklen     TYPE n LENGTH 4,   " Template Length
        convexit    TYPE c LENGTH 5,   " Conversion Routine
        headlen     TYPE n LENGTH 2,   " Heading Length
        scrlen1     TYPE n LENGTH 2,   " Short Field Label Length
        scrlen2     TYPE n LENGTH 2,   " Medium Field Label Length
        scrlen3     TYPE n LENGTH 2,   " Long Field Label Length
        fieldtext   TYPE c LENGTH 60,  " Short Description
        reptext     TYPE c LENGTH 55,  " Heading
        scrtext_s   TYPE c LENGTH 10,  " Short Field Label
        scrtext_m   TYPE c LENGTH 20,  " Medium Field Label
        scrtext_l   TYPE c LENGTH 40,  " Long Field Label
        keyflag     TYPE c LENGTH 1,   " Key Field Indicator
        lowercase   TYPE c LENGTH 1,   " Lowercase Allowed
        mac         TYPE c LENGTH 1,   " Search Help Attached
        genkey      TYPE c LENGTH 1,   " Flag (X or Blank)
        noforkey    TYPE c LENGTH 1,   " Flag (X or Blank)
        valexi      TYPE c LENGTH 1,   " Fixed Values Exist
        noauthch    TYPE c LENGTH 1,   " Flag (X or Blank)
        sign        TYPE c LENGTH 1,   " Sign Flag
        dynpfld     TYPE c LENGTH 1,   " Field Displayed on Dynpro
        f4availabl  TYPE c LENGTH 1,   " Input Help Available
        comptype    TYPE c LENGTH 1,   " Component Type
        lfieldname  TYPE c LENGTH 132, " Long Field Name
        ltrflddis   TYPE c LENGTH 1,   " Left-to-Right Write Direction
        bidictrlc   TYPE c LENGTH 1,   " No BIDI Character Filtering
        outputstyle TYPE n LENGTH 2,   " Output Style (Decfloat Types)
        nohistory   TYPE c LENGTH 1,   " Input History Deactivated
        ampmformat  TYPE c LENGTH 1,   " AM/PM Time Format Indicator
      END OF ty_s_dfies_2.

    TYPES ty_t_dfies_2 TYPE STANDARD TABLE OF ty_s_dfies_2 WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_shlp_intdescr,
        issimple         TYPE c LENGTH 1,   " Elementary Search Help Flag
        hotkey           TYPE c LENGTH 1,   " Hot Key
        selmtype         TYPE c LENGTH 1,   " Category of Selection Method
        selmethod        TYPE c LENGTH 30,  " Selection Method Name
        texttab          TYPE c LENGTH 30,  " Text Table Name
        selmexit         TYPE c LENGTH 30,  " Search Help Exit
        dialogtype       TYPE c LENGTH 1,   " Dialog Type
        ddlanguage       TYPE c LENGTH 1,   " Language Key
        ddtext           TYPE c LENGTH 60,  " Short Text
        dialoginfo       TYPE c LENGTH 1,   " Flag: SELFIELDS/LISTFIELDS read
        f4state          TYPE c LENGTH 1,   " Internal Usage Only
        tabname          TYPE c LENGTH 30,  " Table Name
        fieldname        TYPE c LENGTH 30,  " Field Name
        title            TYPE c LENGTH 60,  " Title Text
        history          TYPE c LENGTH 1,   " Deprecated Usage
        handle           TYPE int4,         " Reference Handle (int4)
        autosuggest      TYPE c LENGTH 1,   " Autosuggest Flag
        fuzzy_search     TYPE c LENGTH 1,   " Fuzzy Search Flag
        fuzzy_similarity TYPE p DECIMALS 1 LENGTH 2, " Accuracy for Fuzzy Search (DEC 2,1)
      END OF ty_shlp_intdescr.

    TYPES:
      BEGIN OF ty_ddshiface,
        shlpfield  TYPE c LENGTH 30,  " Field Name for Pass by Value to F4 Help
        valtabname TYPE c LENGTH 30,  " Structure Name for Input Help Assignment
        valfield   TYPE c LENGTH 132, " Field for Input Help Assignment
        value      TYPE c LENGTH 132, " Field Content from Dynpro
        internal   TYPE c LENGTH 1,   " Flag: Internal Representation
        dispfield  TYPE c LENGTH 1,   " Display-Only Field Flag
        f4field    TYPE c LENGTH 1,   " F4 Pressed Flag
        topshlpnam TYPE c LENGTH 30,  " Higher-Level Search Help Name
        topshlpfld TYPE c LENGTH 30,  " Field of Higher-Level Search Help
      END OF ty_ddshiface.

    TYPES:
      BEGIN OF ty_ddshfprop,
        fieldname  TYPE c LENGTH 30, " Name of Search Help Parameter
        shlpinput  TYPE c LENGTH 1,  " IMPORT Parameter Flag
        shlpoutput TYPE c LENGTH 1,  " EXPORT Parameter Flag
        shlpselpos TYPE n LENGTH 2,  " Position in Dialog Box
        shlplispos TYPE n LENGTH 2,  " Position in Hit List
        shlpseldis TYPE c LENGTH 1,  " Display Field in Dialog Box
        defaultval TYPE c LENGTH 21, " Default Value
      END OF ty_ddshfprop.

    TYPES:
      BEGIN OF ty_ddshselopt,
        shlpname  TYPE c LENGTH 30, " Name of Search Help
        shlpfield TYPE c LENGTH 30, " Name of Search Help Parameter
        sign      TYPE c LENGTH 1,  " Include/Exclude Flag (I/E)
        option    TYPE c LENGTH 2,  " Selection Option (EQ/BT/CP/..)
        low       TYPE c LENGTH 45, " Low Value for Selection
        high      TYPE c LENGTH 45, " High Value for Selection
      END OF ty_ddshselopt.

    TYPES:
      BEGIN OF ty_ddshtextsearch_field,
        fieldname TYPE c LENGTH 30, " Name of a searchable field (freely chosen, not specified by SAP)
      END OF ty_ddshtextsearch_field.

    TYPES tt_ddshtextsearch_fields TYPE STANDARD TABLE OF ty_ddshtextsearch_field WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_ddshtextsearch,
        request TYPE c LENGTH 60,              " Text Search Request
        fields  TYPE tt_ddshtextsearch_fields, " Fields eligible for text search
      END OF ty_ddshtextsearch.

    TYPES:
      BEGIN OF ty_shlp_descr,
        shlpname   TYPE c LENGTH 30,       " Name of a Search Help
        shlptype   TYPE c LENGTH 2,        " Type of an input help (fixed values)
        intdescr   TYPE ty_shlp_intdescr,  " Placeholder for Internal Info of Search Help
        interface  TYPE STANDARD TABLE OF ty_ddshiface WITH DEFAULT KEY,                      " Placeholder for Interface of Search Help
        fielddescr TYPE STANDARD TABLE OF ty_s_dfies_2 WITH DEFAULT KEY,
        fieldprop  TYPE STANDARD TABLE OF ty_ddshfprop WITH DEFAULT KEY,
        selopt     TYPE STANDARD TABLE OF ty_ddshselopt WITH DEFAULT KEY,
        textsearch TYPE ty_ddshtextsearch,
      END OF ty_shlp_descr.

    CLASS-METHODS bus_tr_read
      RETURNING
        VALUE(mt_data) TYPE ty_t_data.

    "! Record the keys of the rows in ir_data on the transport request.
    "! Nothing is recorded and nothing committed on ABAP Cloud or for an
    "! empty ir_data. Raises z2ui5_cx_popup_error when the request refuses
    "! the entries - in both modes without a ROLLBACK WORK and without a
    "! COMMIT: what was recorded so far is still in the caller's LUW, the
    "! caller rolls it back.
    "! @parameter iv_commit | abap_true (the default) ends with COMMIT WORK AND
    "!                       WAIT - which commits whatever the caller has open
    "!                       as well; abap_false leaves the commit to the
    "!                       caller, so the entries and the caller's own
    "!                       database changes are one LUW that one ROLLBACK
    "!                       undoes
    CLASS-METHODS bus_tr_add
      IMPORTING
        ir_data      TYPE REF TO data
        iv_tabname   TYPE string
        is_transport TYPE ty_s_transport
        iv_commit    TYPE abap_bool DEFAULT abap_true.

    "! iv_max_rows - at most this many rows (F4IF_SELECT_VALUES-MAXRECORDS);
    "! 0 reads all of them, as before the parameter existed
    CLASS-METHODS bus_search_help_read
      IMPORTING
        iv_max_rows    TYPE i DEFAULT 0
      CHANGING
        ms_shlp        TYPE ty_shlp_descr
        mv_fname       TYPE string
        mv_table       TYPE string
        mr_data        TYPE REF TO data
        mt_result_desc TYPE ty_t_dfies_2
        mv_shlpfield   TYPE string
        mt_data        TYPE REF TO data
        ms_data_row    TYPE REF TO data.

    CLASS-METHODS file_get_mimetype
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    " abap_true when one component of a row holds the search text - the
    " per-field half of itab_filter_by_val. A component that is not printable
    " (a table of children, a reference) holds no text and answers false
    CLASS-METHODS itab_filter_check_field
      IMPORTING
        field         TYPE any
        search        TYPE string
        ignore_case   TYPE abap_bool
      RETURNING
        VALUE(result) TYPE abap_bool.

    " Every public, non-static, non-constant attribute of an exception that
    " has a printable value - the class-specific payload the text alone does
    " not carry (source/target type of a cast error, the offending value of a
    " conversion error, DB object of an SQL error, ...). The general cx_root
    " attributes (PREVIOUS, TEXTID, IS_RESUMABLE, KERNEL_ERRID) are skipped -
    " the caller renders them itself or they carry no information.
    CLASS-METHODS error_get_attributes
      IMPORTING
        val           TYPE REF TO cx_root
      RETURNING
        VALUE(result) TYPE ty_t_name_value.

    " The source-code position an exception was raised at, as
    " `<program> / <include> / line <n>`; empty when the runtime supplies
    " none. Wraps cx_root->get_source_position so the dependency on the SAP
    " standard exception API stays inside this class. This is what identifies
    " the failing method for exceptions that carry no text of their own -
    " a CX_SY_MOVE_CAST_ERROR only says which types did not match, the
    " position says where.
    CLASS-METHODS error_get_source_position
      IMPORTING
        val           TYPE REF TO cx_root
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.

    CLASS-METHODS msg_get_internal
      IMPORTING
        val           TYPE any
        " the RAP-shape check of a structure is a property of its TYPE - a
        " table decides it once for its rows (msg_get_internal_tab) and hands
        " the answer down, instead of asking for every row
        iv_check_rap  TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result) TYPE ty_t_msg.

    CLASS-METHODS msg_get_by_oref
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE ty_t_msg.

    CLASS-METHODS check_is_rap_struct
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS msg_get_rap
      IMPORTING
        val           TYPE any
        entity_name   TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_t_msg.

    CLASS-METHODS msg_get_rap_row
      IMPORTING
        val         TYPE any
        entity_name TYPE string OPTIONAL
      EXPORTING
        messages    TYPE ty_t_msg
        is_row      TYPE abap_bool.

    CLASS-METHODS msg_get_rap_element
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS msg_get_rap_state_area
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS msg_get_rap_action
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS msg_get_rap_pid
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS msg_get_rap_cid
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS msg_get_rap_tky
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS msg_get_rap_flatten
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_comp_str
      IMPORTING
        val           TYPE any
        comp          TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS scan_flag_prefix
      IMPORTING
        val           TYPE any
        prefix        TYPE clike
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS msg_get_rap_meta
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE ty_t_name_value.

    CLASS-METHODS msg_get_rap_fail_text
      IMPORTING
        cause         TYPE i
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS msg_get_rap_cause_value
      IMPORTING
        name          TYPE clike
      RETURNING
        VALUE(result) TYPE i.

    "! Any elementary value as its text, and never an exception.
    CLASS-METHODS data_get_string
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

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
        line             TYPE any
        dfies            TYPE ty_t_dfies
      RETURNING
        VALUE(rv_tabkey) TYPE trobj_name.

    CLASS-METHODS _read_e070
      CHANGING
        mt_data TYPE ty_t_data.

    CLASS-METHODS set_mandt
      IMPORTING
        ir_data TYPE REF TO data.

    CLASS-METHODS rtti_get_t_attri_on_prem
      IMPORTING
        tabname       TYPE string
      RETURNING
        VALUE(result) TYPE ty_t_dfies.

    CLASS-METHODS rtti_get_t_attri_on_cloud
      IMPORTING
        tabname       TYPE string
      RETURNING
        VALUE(result) TYPE ty_t_dfies ##NEEDED.

  PRIVATE SECTION.

    " whether a type kind is a number: i, int1, int2, int8, p, f,
    " decfloat16, decfloat34 - the one list the number checks share. int8
    " by its value `8`, a constant lower releases do not have
    CLASS-METHODS rtti_check_typekind_number
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS rtti_get_dtel_texts_by_ddic
      IMPORTING
        name        TYPE string
      EXPORTING
        texts       TYPE ty_s_data_element_text
        do_fallback TYPE abap_bool.

    CLASS-METHODS rtti_get_dtel_texts_by_xco
      IMPORTING
        name        TYPE string
      EXPORTING
        texts       TYPE ty_s_data_element_text
        do_fallback TYPE abap_bool.

    TYPES:
      BEGIN OF ty_s_bool_cache,
        typedescr TYPE REF TO cl_abap_typedescr,
        is_bool   TYPE abap_bool,
      END OF ty_s_bool_cache.

    CLASS-DATA mt_bool_cache TYPE HASHED TABLE OF ty_s_bool_cache WITH UNIQUE KEY typedescr.

    TYPES:
      BEGIN OF ty_s_attri_cache,
        absolute_name TYPE string,
        o_struct      TYPE REF TO cl_abap_structdescr,
        t_attri       TYPE cl_abap_structdescr=>component_table,
      END OF ty_s_attri_cache.

    CLASS-DATA mt_attri_cache TYPE HASHED TABLE OF ty_s_attri_cache WITH UNIQUE KEY absolute_name.

    CLASS-DATA gv_check_cloud TYPE abap_bool.

    CLASS-DATA gv_check_cloud_cached TYPE abap_bool.

    " Guards the cycle z2ui5_cx_popup_error=>constructor -> uuid_get_c32 ->
    " RAISE z2ui5_cx_popup_error -> ... see uuid_get_c32.
    CLASS-DATA gv_uuid_failed TYPE abap_bool.

    CLASS-METHODS check_msg_container
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    " a token template of filter_get_token_range_mapping with {LOW} and
    " {HIGH} filled in as written
    CLASS-METHODS filter_token_fill
      IMPORTING
        template      TYPE string
        low           TYPE string
        high          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! the public attributes of an object that carry a message part (TEXT,
    "! TYPE, MSGV1 ...), mapped over is_msg - see msg_map
    CLASS-METHODS msg_get_by_oref_attri
      IMPORTING
        io_obj        TYPE REF TO object
        is_msg        TYPE ty_s_msg
      RETURNING
        VALUE(result) TYPE ty_s_msg.

    "! the rows of a table, the RAP shape decided on the first structured row
    CLASS-METHODS msg_get_internal_tab
      IMPORTING
        it_tab        TYPE ANY TABLE
      RETURNING
        VALUE(result) TYPE ty_t_msg.

    " A value as an Open SQL character literal for a dynamic WHERE: in
    " single quotes, a quote inside doubled.
    CLASS-METHODS filter_sql_quote
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    " codepage: the IANA name CL_ABAP_CONV_CODEPAGE takes; encoding: the
    " name or SAP code page number CL_ABAP_CONV_IN_CE takes. invalid is set
    " when a converter ran and rejected the bytes; no converter at all
    " raises UNSUPPORTED_CODEPAGE_API.
    CLASS-METHODS conv_get_string_by_xstring_cp
      IMPORTING
        val      TYPE xstring
        codepage TYPE string
        encoding TYPE string
      EXPORTING
        result   TYPE string
        invalid  TYPE abap_bool.

    " the text half of itab_filter_check_field: abap_true when text holds
    " search - upper case already when ignore_case is set
    CLASS-METHODS itab_filter_check_text
      IMPORTING
        !text         TYPE string
        search        TYPE string
        ignore_case   TYPE abap_bool
      RETURNING
        VALUE(result) TYPE abap_bool.

    " the bytes of a percent-encoded text (a data URI payload): an escape is
    " its byte, the text between escapes its UTF-8 bytes; `+` is a plus
    CLASS-METHODS conv_get_xstring_by_percent
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE xstring.
ENDCLASS.


CLASS z2ui5_cl_popup_context IMPLEMENTATION.

  METHOD class_constructor.

    cv_char_util_newline        = cl_abap_char_utilities=>newline.
    cv_char_util_cr_lf          = cl_abap_char_utilities=>cr_lf.
    cv_char_util_horizontal_tab = cl_abap_char_utilities=>horizontal_tab.
    cv_char_util_charsize       = cl_abap_char_utilities=>charsize.
    cv_format_e_xml_attr        = cl_abap_format=>e_xml_attr.

    cv_typedescr_typekind_table      = cl_abap_typedescr=>typekind_table.
    cv_typedescr_typekind_dref       = cl_abap_typedescr=>typekind_dref.
    cv_typedescr_typekind_oref       = cl_abap_typedescr=>typekind_oref.
    cv_typedescr_typekind_struct1    = cl_abap_typedescr=>typekind_struct1.
    cv_typedescr_typekind_struct2    = cl_abap_typedescr=>typekind_struct2.
    cv_typedescr_typekind_date       = cl_abap_typedescr=>typekind_date.
    cv_typedescr_typekind_time       = cl_abap_typedescr=>typekind_time.
    cv_typedescr_typekind_packed     = cl_abap_typedescr=>typekind_packed.
    cv_typedescr_kind_struct         = cl_abap_typedescr=>kind_struct.
    cv_typedescr_kind_elem           = cl_abap_typedescr=>kind_elem.
    cv_typedescr_kind_ref            = cl_abap_typedescr=>kind_ref.
    cv_objectdescr_public            = cl_abap_objectdescr=>public.

  ENDMETHOD.

  METHOD boolean_abap_2_json.
      DATA temp1 TYPE string.

    IF boolean_check_by_data( val ) IS NOT INITIAL.

      IF val = abap_true.
        temp1 = `true`.
      ELSE.
        temp1 = `false`.
      ENDIF.
      result = temp1.
    ELSE.
      result = val.
    ENDIF.

  ENDMETHOD.

  METHOD boolean_check_by_data.
        DATA lo_descr TYPE REF TO cl_abap_typedescr.
        FIELD-SYMBOLS <ls_bool_cache> TYPE z2ui5_cl_popup_context=>ty_s_bool_cache.
        DATA temp2 TYPE z2ui5_cl_popup_context=>ty_s_bool_cache.

    TRY.

        lo_descr = cl_abap_elemdescr=>describe_by_data( val ).

        " all supported boolean types are character-like flags, this check
        " filters out every other type before the cache lookup
        IF lo_descr->type_kind <> cl_abap_typedescr=>typekind_char.
          RETURN.
        ENDIF.

        " type descriptors are singletons, so the reference identifies the
        " type without converting/hashing the absolute name on every call

        READ TABLE mt_bool_cache WITH KEY typedescr = lo_descr ASSIGNING <ls_bool_cache>.
        IF sy-subrc = 0.
          result = <ls_bool_cache>-is_bool.
          RETURN.
        ENDIF.

        result = boolean_check_by_name( lo_descr->get_relative_name( ) ).


        CLEAR temp2.
        temp2-typedescr = lo_descr.
        temp2-is_bool = result.
        INSERT temp2 INTO TABLE mt_bool_cache.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD boolean_check_by_name.

    CASE val.
      WHEN `ABAP_BOOL`
          OR `XSDBOOLEAN`
          OR `FLAG`
          OR `XFLAG`
          OR `XFELD`
          OR `ABAP_BOOLEAN`
          OR `WDY_BOOLEAN`
          OR `BOOLE_D`
          OR `OS_BOOLEAN`.
        result = abap_true.
    ENDCASE.

  ENDMETHOD.

  METHOD conv_copy_ref_data.

    FIELD-SYMBOLS <from>   TYPE data.
    FIELD-SYMBOLS <result> TYPE data.

    IF rtti_check_ref_data( from ) IS NOT INITIAL.
      ASSIGN from->* TO <from>.
      IF <from> IS NOT ASSIGNED.
        " unbound data reference - nothing to copy, return an initial reference
        RETURN.
      ENDIF.
    ELSE.
      ASSIGN from TO <from>.
    ENDIF.
    CREATE DATA result LIKE <from>.
    ASSIGN result->* TO <result>.

    <result> = <from>.

  ENDMETHOD.

  METHOD conv_get_xstring_by_data_uri.

    result = conv_get_file_by_data_uri( val )-content.

  ENDMETHOD.

  METHOD conv_get_file_by_data_uri.

    DATA lv_header TYPE string.
    DATA lv_data   TYPE string.

    DATA temp3 TYPE string.
    DATA lv_val LIKE temp3.
    DATA lv_base64 LIKE abap_true.
      TYPES temp1 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_part TYPE temp1.
      DATA temp4 LIKE LINE OF lt_part.
      DATA lr_part LIKE REF TO temp4.
        DATA lv_part TYPE string.
    temp3 = val.

    lv_val = temp3.
    IF lv_val NS `,`.
      " a bare base64 payload, without the `data:...;base64,` prefix - the
      " SPLIT put the whole value into the metadata half and the payload
      " decoded to an empty xstring without a word
      result-content = conv_decode_x_base64( lv_val ).
      RETURN.
    ENDIF.
    SPLIT lv_val AT `,` INTO lv_header lv_data.

    " data:[<mime type>][;charset=...][;base64] - the payload used to be
    " base64-decoded whatever the header said, so a percent-encoded one
    " (`data:text/plain,a%20b`) came out as garbage

    lv_base64 = abap_true.
    IF strlen( lv_header ) >= 5 AND to_lower( lv_header(5) ) = `data:`.
      lv_base64 = abap_false.


      SPLIT substring( val = lv_header
                       off = 5 ) AT `;` INTO TABLE lt_part.


      LOOP AT lt_part REFERENCE INTO lr_part.

        lv_part = to_lower( c_trim( lr_part->* ) ).
        IF sy-tabix = 1.
          result-mime_type = lv_part.
        ELSEIF lv_part = `base64`.
          lv_base64 = abap_true.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lv_base64 = abap_true.
      result-content = conv_decode_x_base64( lv_data ).
    ELSE.
      result-content = conv_get_xstring_by_percent( lv_data ).
    ENDIF.

    IF ( result-mime_type IS INITIAL OR result-mime_type = `application/octet-stream` )
        AND file_name IS NOT INITIAL.
      result-mime_type = file_get_mimetype( file_name ).
    ENDIF.

  ENDMETHOD.

  METHOD conv_get_xstring_by_percent.

    " the bytes as hex digits, converted in one step at the end - a byte
    " string has no expression that appends to it (see url_decode)
    DATA lv_hex TYPE string.
    DATA lv_run TYPE string.
    DATA lv_x   TYPE x LENGTH 1.

    DATA lv_len TYPE i.
    DATA lv_pos TYPE i.
      DATA lv_char TYPE string.
      DATA lv_code TYPE string.
          DATA lv_run_x TYPE xstring.
    lv_len = strlen( val ).

    lv_pos = 0.
    WHILE lv_pos < lv_len.

      lv_char = substring( val = val
                                 off = lv_pos
                                 len = 1 ).

      lv_code = ``.
      IF lv_char = `%` AND lv_pos + 2 < lv_len.
        lv_code = to_upper( substring( val = val
                                       off = lv_pos + 1
                                       len = 2 ) ).
      ENDIF.
      IF lv_code IS NOT INITIAL AND lv_code CO `0123456789ABCDEF`.
        IF lv_run IS NOT INITIAL.

          lv_run_x = conv_get_xstring_by_string( lv_run ).
          lv_hex = |{ lv_hex }{ lv_run_x }|.
          CLEAR lv_run.
        ENDIF.
        lv_x = lv_code.
        lv_hex = |{ lv_hex }{ lv_x }|.
        lv_pos = lv_pos + 3.
        CONTINUE.
      ENDIF.
      lv_run = lv_run && lv_char.
      lv_pos = lv_pos + 1.
    ENDWHILE.
    IF lv_run IS NOT INITIAL.
      lv_run_x = conv_get_xstring_by_string( lv_run ).
      lv_hex = |{ lv_hex }{ lv_run_x }|.
    ENDIF.

    result = lv_hex.

  ENDMETHOD.

  METHOD c_trim.
      DATA lv_before LIKE result.

    result = val.
    " spaces and tabs alternate at either end (`\t \tx`) - one pass of each
    " leaves the inner layer standing, so strip until nothing changes. No
    " fixed number of passes: ten left the eleventh layer of a padded value
    " standing. Every pass that changes something shortens the string, so
    " the loop ends
    DO.

      lv_before = result.
      result = shift_left( shift_right( result ) ).
      result = shift_right( val = result
                            sub = cv_char_util_horizontal_tab ).
      result = shift_left( val = result
                           sub = cv_char_util_horizontal_tab ).
      IF result = lv_before.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD c_trim_lower.

    result = to_lower( c_trim( val ) ).

  ENDMETHOD.

  METHOD filter_itab.

    " the same filter as itab_filter_by_t_range - a field whose range is
    " empty keeps every row there too
    itab_filter_by_t_range( EXPORTING val = filter
                            CHANGING  tab = val ).

  ENDMETHOD.

  METHOD filter_get_multi_by_data.

    DATA temp5 TYPE abap_component_tab.
    DATA temp1 LIKE LINE OF temp5.
    DATA lr_comp LIKE REF TO temp1.
      DATA temp6 TYPE z2ui5_cl_popup_context=>ty_s_filter_multi.
    temp5 = rtti_get_t_attri_by_any( val ).


    LOOP AT temp5 REFERENCE INTO lr_comp.

      CLEAR temp6.
      temp6-name = lr_comp->name.
      INSERT temp6 INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD filter_get_range_by_token.
    DATA lv_value LIKE val.
    DATA lv_length TYPE i.
      DATA temp7 TYPE z2ui5_cl_popup_context=>ty_s_range-sign.
      DATA temp8 TYPE z2ui5_cl_popup_context=>ty_s_range-option.
    lv_value = val.
    IF lv_value IS INITIAL.
      RETURN.
    ENDIF.

    lv_length = strlen( lv_value ).

    " `!(...)`: the negated token filter_get_token_t_by_range_t renders for
    " an excluding row and for NE / NB / NP - the inner token with the sign
    " flipped (I NE x comes back as the equivalent E EQ x, E NE x as I EQ x)
    IF lv_length > 3 AND lv_value(2) = `!(` AND substring( val = lv_value
                                                           off = lv_length - 1 ) = `)`.
      result = filter_get_range_by_token( substring( val = lv_value
                                                     off = 2
                                                     len = lv_length - 3 ) ).

      IF result-sign = `E`.
        temp7 = `I`.
      ELSE.
        temp7 = `E`.
      ENDIF.
      result-sign = temp7.
      RETURN.
    ENDIF.

    result-sign = `I`.

    " the empty-value token of filter_get_token_range_mapping - read as `<`
    " it was LT `leer>`
    IF lv_value = `<leer>`.
      result-option = `EQ`.
      RETURN.
    ENDIF.

    IF lv_length >= 2 AND ( lv_value(2) = `<=` OR lv_value(2) = `>=` ).

      IF lv_value(1) = `<`.
        temp8 = `LE`.
      ELSE.
        temp8 = `GE`.
      ENDIF.
      result-option = temp8.
      result-low    = substring( val = lv_value
                                 off = 2 ).

    ELSEIF lv_value(1) = `=`.
      result-option = `EQ`.
      result-low    = substring( val = lv_value
                                 off = 1 ).

    ELSEIF lv_value(1) = `<`.
      result-option = `LT`.
      result-low    = substring( val = lv_value
                                 off = 1 ).

    ELSEIF lv_value(1) = `>`.
      result-option = `GT`.
      result-low    = substring( val = lv_value
                                 off = 1 ).

    ELSEIF lv_value CA `*`.
      " the pattern as it was typed: `*abc*` contains, `abc*` starts with,
      " `*abc` ends with. LOW keeps the asterisks - they are what makes a CP
      " row a pattern, for IN as well as for the LIKE built from it
      result-option = `CP`.
      result-low    = lv_value.

    ELSEIF lv_value CS `...`.
      SPLIT lv_value AT `...` INTO result-low result-high.
      result-option = `BT`.

    ELSE.
      result-option = `EQ`.
      result-low    = lv_value.
    ENDIF.
  ENDMETHOD.

  METHOD filter_get_range_t_by_token_t.

    DATA ls_token LIKE LINE OF val.
    LOOP AT val INTO ls_token.
      INSERT filter_get_range_by_token( ls_token-text ) INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD filter_get_token_range_mapping.

    DATA temp9 TYPE z2ui5_cl_popup_context=>ty_t_name_value.
    DATA temp10 LIKE LINE OF temp9.
    CLEAR temp9.

    temp10-n = `EQ`.
    temp10-v = `={LOW}`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `LT`.
    temp10-v = `<{LOW}`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `LE`.
    temp10-v = `<={LOW}`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `GT`.
    temp10-v = `>{LOW}`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `GE`.
    temp10-v = `>={LOW}`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `CP`.
    temp10-v = `*{LOW}*`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `BT`.
    temp10-v = `{LOW}...{HIGH}`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `NB`.
    temp10-v = `!({LOW}...{HIGH})`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `NE`.
    temp10-v = `!(={LOW})`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `NP`.
    temp10-v = `!(*{LOW}*)`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `!<leer>`.
    temp10-v = `!(<leer>)`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `<leer>`.
    temp10-v = `<leer>`.
    INSERT temp10 INTO TABLE temp9.
    result = temp9.

  ENDMETHOD.

  METHOD filter_get_token_t_by_range_t.

    DATA lt_mapping TYPE z2ui5_cl_popup_context=>ty_t_name_value.
    DATA lt_tab TYPE ty_t_range.
    FIELD-SYMBOLS <lt_source> TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_range> TYPE ty_s_range.
    FIELD-SYMBOLS <lv_part> TYPE any.
      DATA lv_index TYPE i.
      FIELD-SYMBOLS <ls_source> TYPE ANY.
    DATA temp11 LIKE LINE OF lt_tab.
    DATA lr_row LIKE REF TO temp11.
      DATA lv_option TYPE string.
      DATA lv_value TYPE string.
      DATA temp12 TYPE string.
      DATA temp13 TYPE z2ui5_cl_popup_context=>ty_s_name_value.
        FIELD-SYMBOLS <temp14> LIKE LINE OF lt_mapping.
        DATA temp15 LIKE sy-tabix.
        DATA temp16 TYPE string.
      DATA temp17 TYPE z2ui5_cl_popup_context=>ty_s_token.
    lt_mapping = filter_get_token_range_mapping( ).



    itab_corresponding( EXPORTING val = val
                        CHANGING  tab = lt_tab ).

    " LOW / HIGH again from the source row, through data_get_string: the
    " move into the string fields above wrote a number the way a MOVE does
    " - sign behind it and padded (`5- `), so a select-option on an integer
    " field showed `=42 ` and `5-...10 `
    " UNASSIGN + IS ASSIGNED, not sy-subrc, in the loop (#1937)



    ASSIGN val TO <lt_source>.
    IF <lt_source> IS ASSIGNED.

      lv_index = 0.

      LOOP AT <lt_source> ASSIGNING <ls_source>.
        lv_index = lv_index + 1.
        UNASSIGN <ls_range>.
        READ TABLE lt_tab INDEX lv_index ASSIGNING <ls_range>.
        IF <ls_range> IS NOT ASSIGNED.
          EXIT.
        ENDIF.
        UNASSIGN <lv_part>.
        ASSIGN COMPONENT `LOW` OF STRUCTURE <ls_source> TO <lv_part>.
        IF <lv_part> IS ASSIGNED.
          <ls_range>-low = data_get_string( <lv_part> ).
        ENDIF.
        UNASSIGN <lv_part>.
        ASSIGN COMPONENT `HIGH` OF STRUCTURE <ls_source> TO <lv_part>.
        IF <lv_part> IS ASSIGNED.
          <ls_range>-high = data_get_string( <lv_part> ).
        ENDIF.
      ENDLOOP.
    ENDIF.



    LOOP AT lt_tab REFERENCE INTO lr_row.

      " an option the mapping does not know - initial (a row appended with
      " sign and low only) or lower-case - used to raise a raw
      " CX_SY_ITAB_LINE_NOT_FOUND out of the whole call; it renders as the
      " equality token, which is what a range row without an option means


      lv_option = to_upper( lr_row->option ).

      CLEAR temp12.

      READ TABLE lt_mapping INTO temp13 WITH KEY n = lv_option.
      IF sy-subrc = 0.
        temp12 = temp13-v.
      ENDIF.
      lv_value = temp12. "#EC CI_SORTSEQ
      IF lv_value IS INITIAL.


        temp15 = sy-tabix.
        READ TABLE lt_mapping WITH KEY n = `EQ` ASSIGNING <temp14>.
        sy-tabix = temp15.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        lv_value = <temp14>-v. "#EC CI_SORTSEQ
      ENDIF.
      IF ( lv_option = `CP` OR lv_option = `NP` ) AND lr_row->low CA `*`.
        " LOW is a pattern already - wrapping it in the template's asterisks
        " would turn `abc*` into `*abc**`

        IF lv_option = `CP`.
          temp16 = lr_row->low.
        ELSE.
          temp16 = |!({ lr_row->low })|.
        ENDIF.
        lv_value = temp16.
      ELSE.
        lv_value = filter_token_fill( template = lv_value
                                      low      = lr_row->low
                                      high     = lr_row->high ).
      ENDIF.

      " an excluding row must not render like its including twin - negate the
      " token so the MultiInput shows the filter's real meaning
      IF lr_row->sign = `E`.
        lv_value = |!({ lv_value })|.
      ENDIF.


      CLEAR temp17.
      temp17-key = lv_value.
      temp17-text = lv_value.
      temp17-visible = abap_true.
      temp17-editable = abap_true.
      INSERT temp17 INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD itab_filter_by_val.
    " TRANSPILER NOTE: ABAP CS is always case-insensitive, so the two match
    " branches below differ deliberately: ignore_case = abap_true uses
    " to_upper + CS (JS: toLowerCase().includes(toLowerCase())), the default
    " uses find( ) for a genuinely case-sensitive match (JS: plain includes()).
    FIELD-SYMBOLS <row>   TYPE any.
    FIELD-SYMBOLS <field> TYPE any.
    FIELD-SYMBOLS <kept>  TYPE STANDARD TABLE.
    " the components that hold text: by position without a field list, by
    " name with one
    TYPES temp2 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA lt_index TYPE temp2.
    DATA lt_name  TYPE string_table.
    DATA lv_whole_row TYPE abap_bool.
    DATA lr_kept TYPE REF TO data.

    DATA temp18 TYPE string.
    DATA lv_search LIKE temp18.
      DATA lv_index TYPE i.
      DATA temp19 LIKE LINE OF fields.
      DATA lr_field LIKE REF TO temp19.
      DATA lv_check_found LIKE abap_false.
      DATA temp20 LIKE LINE OF lt_index.
      DATA lr_index LIKE REF TO temp20.
      DATA temp21 LIKE LINE OF lt_name.
      DATA lr_name LIKE REF TO temp21.
    IF ignore_case = abap_true.
      temp18 = to_upper( val ).
    ELSE.
      temp18 = val.
    ENDIF.

    lv_search = temp18.
    " an empty search is no filter: every row stays. Said here rather than
    " left to the match below - whether find( ) or CS reports a hit for an
    " empty substring is not something to build on, and a row with nothing
    " printable in it (only a table of children, only a reference) matched
    " nothing and was deleted
    IF lv_search IS INITIAL.
      RETURN.
    ENDIF.

    " Which component holds text is a property of the line type: decided
    " once, on the first row, instead of describing every cell of every
    " row - this runs on every search keystroke over the whole table. A
    " table, a deep structure or a reference cannot go into a string
    " template - such a component holds no text to match
    READ TABLE tab INDEX 1 ASSIGNING <row>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    IF fields IS INITIAL.

      lv_index = 1.
      DO.
        ASSIGN COMPONENT lv_index OF STRUCTURE <row> TO <field>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        IF rtti_check_printable( <field> ) = abap_true.
          INSERT lv_index INTO TABLE lt_index.
        ENDIF.
        lv_index = lv_index + 1.
      ENDDO.
      " elementary line type (e.g. string_table) has no components -
      " match against the whole line instead of deleting every row
      IF lv_index = 1.
        lv_whole_row = rtti_check_printable( <row> ).
      ENDIF.
    ELSE.


      LOOP AT fields REFERENCE INTO lr_field.
        ASSIGN COMPONENT lr_field->* OF STRUCTURE <row> TO <field>.
        IF sy-subrc = 0 AND rtti_check_printable( <field> ) = abap_true.
          INSERT lr_field->* INTO TABLE lt_name.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " the matching rows go into a copy - a DELETE ... INDEX per dropped row
    " shifted the rest of the table every time
    CREATE DATA lr_kept LIKE tab.
    ASSIGN lr_kept->* TO <kept>.

    LOOP AT tab ASSIGNING <row>.


      lv_check_found = abap_false.
      IF lv_whole_row = abap_true.
        lv_check_found = itab_filter_check_text( text        = |{ <row> }|
                                                 search      = lv_search
                                                 ignore_case = ignore_case ).
      ENDIF.


      LOOP AT lt_index REFERENCE INTO lr_index.
        ASSIGN COMPONENT lr_index->* OF STRUCTURE <row> TO <field>.
        IF itab_filter_check_text( text        = |{ <field> }|
                                   search      = lv_search
                                   ignore_case = ignore_case ) = abap_true.
          lv_check_found = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.


      LOOP AT lt_name REFERENCE INTO lr_name.
        ASSIGN COMPONENT lr_name->* OF STRUCTURE <row> TO <field>.
        IF itab_filter_check_text( text        = |{ <field> }|
                                   search      = lv_search
                                   ignore_case = ignore_case ) = abap_true.
          lv_check_found = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_check_found = abap_true.
        INSERT <row> INTO TABLE <kept>.
      ENDIF.

    ENDLOOP.

    IF lines( <kept> ) <> lines( tab ).
      tab = <kept>.
    ENDIF.

  ENDMETHOD.

  METHOD rtti_check_ref_data.
        DATA lo_typdescr TYPE REF TO cl_abap_typedescr.
        DATA temp22 TYPE REF TO cl_abap_refdescr.
        DATA lo_referenced TYPE REF TO cl_abap_typedescr.
        DATA temp1 TYPE xsdboolean.

    TRY.
        " a kind comparison, not a CAST probe: the cast raised
        " CX_SY_MOVE_CAST_ERROR for every NON-reference value, and the
        " non-reference is the COMMON case on this path (every value routed
        " through conv_copy_ref_data asks) - deciding a type question by
        " exception is orders of magnitude more expensive than comparing.
        " cl_abap_refdescr covers data and object references alike, and so
        " does kind_ref

        lo_typdescr = cl_abap_typedescr=>describe_by_data( val ).
        IF lo_typdescr->kind <> cl_abap_typedescr=>kind_ref.
          RETURN.
        ENDIF.
        " kind_ref covers object references too, and the one caller that
        " dereferences on a true answer (conv_copy_ref_data: `from->*`)
        " cannot do that to an object reference. Only a reference to DATA
        " answers true

        temp22 ?= lo_typdescr.

        lo_referenced = temp22->get_referenced_type( ).

        temp1 = boolc( lo_referenced->kind <> cl_abap_typedescr=>kind_class AND lo_referenced->kind <> cl_abap_typedescr=>kind_intf ).
        result = temp1.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_get_classname_by_ref.

    " an unbound reference has no class - answer with an empty name instead
    " of letting the RTTI call fail. Callers ask this while rendering (error
    " context, response header), where a half-built object graph is normal
    IF val IS NOT BOUND.
      RETURN.
    ENDIF.

    result = substring_after( val = cl_abap_classdescr=>get_class_name( val )
                              sub = `\CLASS=` ).

  ENDMETHOD.

  METHOD rtti_get_type_kind.

    result = cl_abap_datadescr=>get_data_type_kind( val ).

  ENDMETHOD.

  METHOD rtti_get_t_attri_by_include.

    " `type` IS the include's descriptor - the component the caller took it
    " from (as_include = abap_true) carries the structure descriptor, and
    " RTTI descriptors are singletons. It used to be re-described by name,
    " an RTTI lookup per include on the binding path that could only hand
    " back the object already in hand - or fail for a local include type
    " whose absolute name does not resolve, where the direct cast cannot
    DATA sdescr TYPE REF TO cl_abap_structdescr.
        DATA lx_cast TYPE REF TO cx_sy_move_cast_error.
    TRY.
        sdescr ?= type.

      CATCH cx_sy_move_cast_error INTO lx_cast.
        RAISE EXCEPTION TYPE z2ui5_cx_popup_error
          EXPORTING
            val      = |Include type '{ type->absolute_name }' is not a structure|
            previous = lx_cast.
    ENDTRY.
    result = expand_components( val   = sdescr->get_components( )
                                depth = depth ).

  ENDMETHOD.

  METHOD expand_components.
    DATA temp23 LIKE LINE OF val.
    DATA lr_comp LIKE REF TO temp23.
        DATA lt_include TYPE abap_component_tab.
          DATA temp24 LIKE LINE OF lt_include.
          DATA lr_include LIKE REF TO temp24.

    " see the declaration: bounded so a cyclic include chain surfaces as a
    " readable error instead of a stack-overflow dump. 16 nested include
    " levels is far beyond any real DDIC structure
    IF depth > 16.
      RAISE EXCEPTION TYPE z2ui5_cx_popup_error
        EXPORTING
          val = `RTTI_INCLUDE_RECURSION - include expansion exceeded 16 levels (cyclic include?)`.
    ENDIF.



    LOOP AT val REFERENCE INTO lr_comp.
      IF lr_comp->as_include = abap_true.
        " INCLUDE ... RENAMING WITH SUFFIX: the structure's components are
        " named <component><suffix>, and every nested level adds its own -
        " the bare include names do not exist on the structure, so a
        " binding path or ASSIGN COMPONENT built from them fails

        lt_include = rtti_get_t_attri_by_include( type  = lr_comp->type
                                                        depth = depth + 1 ).
        IF lr_comp->suffix IS NOT INITIAL.


          LOOP AT lt_include REFERENCE INTO lr_include.
            lr_include->name = lr_include->name && lr_comp->suffix.
          ENDLOOP.
        ENDIF.
        INSERT LINES OF lt_include INTO TABLE result.
      ELSE.
        INSERT lr_comp->* INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD rtti_get_t_attri_by_oref.

    DATA lo_descr TYPE REF TO cl_abap_typedescr.
    DATA temp25 TYPE REF TO cl_abap_classdescr.

    " describe_by_object_ref raises the classic exception REFERENCE_IS_INITIAL
    " for an initial reference - called functionally that is a runtime error no
    " CATCH reaches
    cl_abap_objectdescr=>describe_by_object_ref( EXPORTING  p_object_ref         = val
                                                 RECEIVING p_descr_ref           = lo_descr
                                                 EXCEPTIONS reference_is_initial = 1
                                                            OTHERS               = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE z2ui5_cx_popup_error
        EXPORTING
          val = `RTTI_REFERENCE_IS_INITIAL`.
    ENDIF.

    temp25 ?= lo_descr.
    result = temp25->attributes.

  ENDMETHOD.

  METHOD rtti_get_t_attri_by_any.

    DATA lo_struct        TYPE REF TO cl_abap_structdescr.
    DATA lo_type          TYPE REF TO cl_abap_typedescr.
    " declared, not CONV string( ) - see error_get_attributes
    DATA lv_absolute_name TYPE string.
        DATA temp26 TYPE REF TO cl_abap_structdescr.
        DATA temp27 TYPE REF TO cl_abap_structdescr.
        DATA temp2 TYPE REF TO cl_abap_tabledescr.
    FIELD-SYMBOLS <ls_attri_cache> TYPE z2ui5_cl_popup_context=>ty_s_attri_cache.
      DATA temp28 TYPE z2ui5_cl_popup_context=>ty_s_attri_cache.

    " describe_by_data never fails for a data object, and the reference case
    " is the one branch. The fallback chain that used to sit here - a second
    " describe_by_data_ref, then describe_by_name( val ) - could rescue no
    " input: the first only re-raised what the branch above had raised, and
    " a data object is no type name
    lo_type = cl_abap_typedescr=>describe_by_data( val ).
    IF lo_type->kind = cl_abap_typedescr=>kind_ref.
      " REFERENCE_IS_INITIAL - see rtti_get_t_attri_by_oref
      cl_abap_typedescr=>describe_by_data_ref( EXPORTING  p_data_ref           = val
                                               RECEIVING p_descr_ref           = lo_type
                                               EXCEPTIONS reference_is_initial = 1
                                                          OTHERS               = 2 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE z2ui5_cx_popup_error
          EXPORTING
            val = `RTTI_REFERENCE_IS_INITIAL`.
      ENDIF.
    ENDIF.

    CASE lo_type->kind.
      WHEN cl_abap_typedescr=>kind_struct.

        temp26 ?= lo_type.
        lo_struct = temp26.
      WHEN cl_abap_typedescr=>kind_table.


        temp2 ?= lo_type.
        temp27 ?= temp2->get_table_line_type( ).
        lo_struct = temp27.
      WHEN OTHERS.
        lo_struct ?= lo_type.
    ENDCASE.

    " descriptor instances are singletons per type, so the identity check
    " guards against absolute names reused by other (local/anonymous) types
    lv_absolute_name = lo_struct->absolute_name.

    READ TABLE mt_attri_cache WITH KEY absolute_name = lv_absolute_name ASSIGNING <ls_attri_cache>.
    IF <ls_attri_cache> IS ASSIGNED AND <ls_attri_cache>-o_struct = lo_struct.
      result = <ls_attri_cache>-t_attri.
      RETURN.
    ENDIF.

    result = expand_components( lo_struct->get_components( ) ).

    IF <ls_attri_cache> IS ASSIGNED.
      <ls_attri_cache>-o_struct = lo_struct.
      <ls_attri_cache>-t_attri  = result.
    ELSE.

      CLEAR temp28.
      temp28-absolute_name = lv_absolute_name.
      temp28-o_struct = lo_struct.
      temp28-t_attri = result.
      INSERT temp28 INTO TABLE mt_attri_cache.
    ENDIF.

  ENDMETHOD.

  METHOD time_get_timestampl.
    GET TIME STAMP FIELD result.
  ENDMETHOD.

  METHOD url_param_get.

    DATA lt_params TYPE z2ui5_cl_popup_context=>ty_t_name_value.
    DATA lv_val TYPE string.
    DATA temp29 TYPE string.
    DATA temp30 TYPE z2ui5_cl_popup_context=>ty_s_name_value.
    lt_params = url_param_get_tab( url ).

    lv_val = c_trim_lower( val ).

    CLEAR temp29.

    READ TABLE lt_params INTO temp30 WITH KEY n = lv_val.
    IF sy-subrc = 0.
      temp29 = temp30-v.
    ENDIF.
    result = temp29.

  ENDMETHOD.

  METHOD url_param_get_tab.

    " a full URL or a request URI carries a path before its query: cut at
    " the FIRST `?` only. A later `?` is a legal, unencoded character of a
    " value (`title=why?`) - cutting there dropped every parameter before
    " it, app_start included, and the shell fell back to the start page
    " declared, not inline: DATA( ) from a generic CLIKE parameter is
    " "fixed type STRING used for generic type CLIKE" in the extended check
    DATA lv_search TYPE string.
    DATA lv_startup TYPE string.
      DATA lv_before TYPE string.
      DATA lv_packed TYPE string.
      DATA lv_rest TYPE string.
    TYPES temp3 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_param TYPE temp3.
    DATA temp31 LIKE LINE OF lt_param.
    DATA lr_param LIKE REF TO temp31.
      DATA lv_name TYPE string.
      DATA lv_value TYPE string.
      DATA temp32 TYPE z2ui5_cl_popup_context=>ty_s_name_value.
    lv_search = val.
    IF lv_search CS `?`.
      lv_search = substring_after( val = lv_search
                                   sub = `?` ).
    ENDIF.

    " The FLP packs the target's own parameters into ONE value,
    " sap-startup-params, with its `=` and `&` percent-encoded. Prepend &
    " before searching so it is also unwrapped when it is the first/only
    " query parameter (typical FLP target mapping). Only THAT value is
    " decoded: decoding the whole query first tore every legitimately
    " encoded `&` or `=` in any other value apart (`a=x%26y` became the
    " two parameters a=x and y=), and app_get_url wrote the damage back
    " into every generated link

    lv_startup = substring_after( val = |&{ lv_search }|
                                        sub = `&sap-startup-params=` ).
    IF lv_startup IS NOT INITIAL.
      " what stands BEFORE the wrapper is as much a parameter as what follows
      " it: `?app_start=x&sap-startup-params=...` used to lose app_start, and
      " a `sap-client` in front of the wrapper was gone from every link
      " app_get_url rebuilt from this table. lv_before keeps the prepended
      " `&`, so the empty first segment it produces below is skipped like a
      " trailing one

      lv_before = substring_before( val = |&{ lv_search }|
                                          sub = `&sap-startup-params=` ).


      SPLIT lv_startup AT `&` INTO lv_packed lv_rest.
      lv_packed = replace( val  = lv_packed
                           sub  = `%3D`
                           with = `=`
                           occ  = 0 ).
      " RFC 3986 allows lowercase hex digits in percent-encodings, so decode
      " %3d the same way as %3D (%26 contains no letters and needs no twin)
      lv_packed = replace( val  = lv_packed
                           sub  = `%3d`
                           with = `=`
                           occ  = 0 ).
      lv_packed = replace( val  = lv_packed
                           sub  = `%26`
                           with = `&`
                           occ  = 0 ).
      lv_search = lv_packed.
      IF lv_rest IS NOT INITIAL.
        lv_search = |{ lv_packed }&{ lv_rest }|.
      ENDIF.
      IF lv_before IS NOT INITIAL.
        lv_search = |{ lv_before }&{ lv_search }|.
      ENDIF.
    ENDIF.



    SPLIT lv_search AT `&` INTO TABLE lt_param.



    LOOP AT lt_param REFERENCE INTO lr_param.


      SPLIT lr_param->* AT `=` INTO lv_name lv_value.
      " an empty segment (empty search string, trailing &) would otherwise
      " produce a phantom nameless parameter that url_param_create_url
      " writes back out as a stray `=&`
      IF lv_name IS INITIAL.
        CONTINUE.
      ENDIF.
      " normalize the name so lookups are case-insensitive on every input
      " shape (with or without a leading path/question mark) - the value
      " keeps its original case

      CLEAR temp32.
      temp32-n = c_trim_lower( lv_name ).
      temp32-v = lv_value.
      INSERT temp32 INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD rtti_get_t_attri_by_table_name.

    DATA lo_obj    TYPE REF TO cl_abap_typedescr.
    DATA lo_struct TYPE REF TO cl_abap_structdescr.
            DATA temp33 TYPE REF TO cl_abap_structdescr.
            DATA temp34 TYPE REF TO cl_abap_structdescr.
            DATA temp3 TYPE REF TO cl_abap_tabledescr.

    IF table_name IS INITIAL.
      RAISE EXCEPTION TYPE z2ui5_cx_popup_error
        EXPORTING
          val = `TABLE_NAME_INITIAL_ERROR`.
    ENDIF.

    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = table_name
                                         RECEIVING  p_descr_ref    = lo_obj
                                         EXCEPTIONS type_not_found = 1
                                                    OTHERS         = 2 ).
    IF sy-subrc <> 0.
      " an unknown name has no columns - the result stays empty, as it always
      " did (the former TABLE_NOT_FOUND raise sat inside its own CATCH cx_root)
      RETURN.
    ENDIF.

    " one describe: a structure (DDIC table, structure, view) or a table
    " type, whose line type is the structure. Anything else has no columns
    TRY.
        CASE lo_obj->kind.
          WHEN cl_abap_typedescr=>kind_struct.

            temp33 ?= lo_obj.
            lo_struct = temp33.
          WHEN cl_abap_typedescr=>kind_table.


            temp3 ?= lo_obj.
            temp34 ?= temp3->get_table_line_type( ).
            lo_struct = temp34.
          WHEN OTHERS.
            RETURN.
        ENDCASE.
      CATCH cx_sy_move_cast_error.
        RETURN.
    ENDTRY.

    " expand_components also applies the RENAMING WITH SUFFIX of an include
    result = expand_components( lo_struct->get_components( ) ).

  ENDMETHOD.

  METHOD itab_corresponding.

    FIELD-SYMBOLS <row_in>  TYPE any.
    FIELD-SYMBOLS <row_out> TYPE any.

    LOOP AT val ASSIGNING <row_in>.
      INSERT INITIAL LINE INTO TABLE tab ASSIGNING <row_out>.
      MOVE-CORRESPONDING <row_in> TO <row_out>.
    ENDLOOP.

  ENDMETHOD.

  METHOD itab_get_by_struc.

    DATA lt_attri TYPE abap_component_tab.
    DATA temp35 LIKE LINE OF lt_attri.
    DATA lr_attri LIKE REF TO temp35.
      FIELD-SYMBOLS <component> TYPE any.
          DATA temp36 TYPE z2ui5_cl_popup_context=>ty_s_name_value.
    lt_attri = rtti_get_t_attri_by_any( val ).


    LOOP AT lt_attri REFERENCE INTO lr_attri.


      ASSIGN COMPONENT lr_attri->name OF STRUCTURE val TO <component>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CASE rtti_get_type_kind( <component> ).

        " skip components that cannot be moved into the string value: tables,
        " nested structures and references would raise an unhandled move
        " error at runtime
        WHEN cl_abap_typedescr=>typekind_table OR
             cl_abap_typedescr=>typekind_struct1 OR
             cl_abap_typedescr=>typekind_struct2 OR
             cl_abap_typedescr=>typekind_dref OR
             cl_abap_typedescr=>typekind_oref.

        WHEN OTHERS.
          " through data_get_string, not a plain MOVE: that writes the sign
          " BEHIND a number, so -5 read `5-` and a packed -12.50 `12.50-`

          CLEAR temp36.
          temp36-n = lr_attri->name.
          temp36-v = data_get_string( <component> ).
          INSERT temp36 INTO TABLE result.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD itab_filter_by_t_range.

    DATA ref TYPE REF TO data.
      DATA ls_filter LIKE LINE OF val.
        FIELD-SYMBOLS <field> TYPE any.

    LOOP AT tab REFERENCE INTO ref.

      LOOP AT val INTO ls_filter.

        IF ls_filter-t_range IS INITIAL.
          CONTINUE.
        ENDIF.


        ASSIGN ref->(ls_filter-name) TO <field>.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        IF <field> NOT IN ls_filter-t_range.
          DELETE tab.
          EXIT.
        ENDIF.

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD msg_get_t.

    result = msg_get_internal( val ).
    IF result IS INITIAL AND val2 IS NOT INITIAL.
      result = msg_get_internal( val2 ).
    ENDIF.

  ENDMETHOD.

  METHOD rtti_check_clike.

    CASE rtti_get_type_kind( val ).
      " typekind_clike/_csequence are generic kinds of formal parameters and
      " can never be returned for a concrete data object - list the concrete
      " character-like kinds (c, string, n, d, t) instead
      WHEN cl_abap_datadescr=>typekind_char OR
          cl_abap_datadescr=>typekind_string OR
          cl_abap_datadescr=>typekind_num OR
          cl_abap_datadescr=>typekind_date OR
          cl_abap_datadescr=>typekind_time.
        result = abap_true.
    ENDCASE.

  ENDMETHOD.

  METHOD rtti_check_printable.
    DATA lv_type_kind TYPE string.
    DATA temp2 TYPE xsdboolean.

    IF rtti_check_clike( val ) = abap_true.
      result = abap_true.
      RETURN.
    ENDIF.


    lv_type_kind = rtti_get_type_kind( val ).

    temp2 = boolc( lv_type_kind = cl_abap_datadescr=>typekind_hex OR rtti_check_typekind_number( lv_type_kind ) = abap_true ).
    result = temp2.

  ENDMETHOD.

  METHOD ui5_get_msg_type.

    " A (abort) and X (exit) are the two SAP message types above E - a
    " BAPIRET2 or SYMSG row of either is an error, never an information
    CASE val.
      WHEN `E` OR `A` OR `X`.
        result = cs_ui5_msg_type-e.
      WHEN `S`.
        result = cs_ui5_msg_type-s.
      WHEN `W`.
        result = cs_ui5_msg_type-w.
      WHEN OTHERS.
        result = cs_ui5_msg_type-i.
    ENDCASE.

  ENDMETHOD.

  METHOD rtti_get_data_element_text_l.

    result = rtti_get_data_element_texts( val )-long.

  ENDMETHOD.

  METHOD rtti_get_ddic_type_name.

    DATA temp37 TYPE REF TO cl_abap_elemdescr.
    temp37 ?= type.
    result = substring_after( val = temp37->absolute_name
                              sub = `\TYPE=` ).

  ENDMETHOD.

  METHOD rtti_create_sel_tab_type.

    DATA lt_comp TYPE cl_abap_structdescr=>component_table.

    FIELD-SYMBOLS <tab> TYPE ANY TABLE.
    DATA lo_table TYPE REF TO cl_abap_tabledescr.
        DATA temp38 TYPE REF TO cl_abap_tabledescr.
        DATA lx_cast TYPE REF TO cx_root.
    DATA lo_line TYPE REF TO cl_abap_datadescr.
        DATA temp39 TYPE REF TO cl_abap_structdescr.
        DATA lo_struct LIKE temp39.
        DATA temp40 TYPE REF TO cl_abap_elemdescr.
        DATA lo_elem LIKE temp40.
        DATA temp41 TYPE abap_componentdescr.
    DATA temp42 LIKE sy-subrc.
      DATA lo_type_bool TYPE REF TO cl_abap_typedescr.
      DATA temp43 TYPE abap_componentdescr.
      DATA temp4 TYPE REF TO cl_abap_datadescr.
    DATA lo_line_type TYPE REF TO cl_abap_structdescr.

    " an unbound reference left <tab> unassigned and describe_by_data dumped
    " with GETWA_NOT_ASSIGNED; a reference to a scalar failed the cast below.
    " Both are the caller's mistake and answer as the framework's exception
    IF ir_tab IS NOT BOUND.
      RAISE EXCEPTION TYPE z2ui5_cx_popup_error
        EXPORTING
          val = `RTTI_CREATE_SEL_TAB_TYPE: no table reference supplied`.
    ENDIF.
    ASSIGN ir_tab->* TO <tab>.


    TRY.

        temp38 ?= cl_abap_typedescr=>describe_by_data( <tab> ).
        lo_table = temp38.

      CATCH cx_root INTO lx_cast.
        RAISE EXCEPTION TYPE z2ui5_cx_popup_error
          EXPORTING
            val = lx_cast.
    ENDTRY.

    " decide the line kind by RTTI instead of by a failing cast: the CATCH
    " branch cast to cl_abap_elemdescr sat outside any TRY, so a table whose
    " line is itself a table or a reference raised CX_SY_MOVE_CAST_ERROR

    lo_line = lo_table->get_table_line_type( ).
    CASE lo_line->kind.
      WHEN cv_typedescr_kind_struct.

        temp39 ?= lo_line.

        lo_struct = temp39.
        lt_comp = lo_struct->get_components( ).
      WHEN cv_typedescr_kind_elem.
        result-check_table_line = abap_true.

        temp40 ?= lo_line.

        lo_elem = temp40.

        CLEAR temp41.
        temp41-name = `TAB_LINE`.
        temp41-type = lo_elem.
        INSERT temp41 INTO TABLE lt_comp.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE z2ui5_cx_popup_error
          EXPORTING
            val = `RTTI_CREATE_SEL_TAB_TYPE: a table of tables or references has no selectable line`.
    ENDCASE.


    READ TABLE lt_comp WITH KEY name = sel_field_name TRANSPORTING NO FIELDS.
    temp42 = sy-subrc.
    IF add_sel_field = abap_true
        AND NOT temp42 = 0. "#EC CI_SORTSEQ

      lo_type_bool = cl_abap_typedescr=>describe_by_name( `ABAP_BOOL` ).

      CLEAR temp43.
      temp43-name = sel_field_name.

      temp4 ?= lo_type_bool.
      temp43-type = temp4.
      INSERT temp43 INTO TABLE lt_comp.
    ENDIF.


    lo_line_type = cl_abap_structdescr=>create( lt_comp ).
    result-tabledescr = cl_abap_tabledescr=>create( lo_line_type ).

  ENDMETHOD.

  METHOD rtti_check_table.

    DATA temp3 TYPE xsdboolean.
    temp3 = boolc( rtti_get_type_kind( val ) = cl_abap_typedescr=>typekind_table ).
    result = temp3.

  ENDMETHOD.

  METHOD rtti_check_structure.
        DATA lo_type TYPE REF TO cl_abap_typedescr.
        DATA temp4 TYPE xsdboolean.

    TRY.

        lo_type = cl_abap_typedescr=>describe_by_data( val ).

        temp4 = boolc( lo_type->kind = cl_abap_typedescr=>kind_struct ).
        result = temp4.
      CATCH cx_root.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_check_typekind_number.

    CASE val.
      WHEN cl_abap_typedescr=>typekind_int
          OR cl_abap_typedescr=>typekind_int1
          OR cl_abap_typedescr=>typekind_int2
          OR cl_abap_typedescr=>typekind_packed
          OR cl_abap_typedescr=>typekind_float
          OR cl_abap_typedescr=>typekind_decfloat16
          OR cl_abap_typedescr=>typekind_decfloat34
          OR `8`.
        result = abap_true.
    ENDCASE.

  ENDMETHOD.

  METHOD data_get_string.

    " the plain assignment is what turns an elementary value into its text
    " - the same trick get_comp_str( ) uses. A value the runtime refuses to
    " convert must not be the reason a box does not appear
    TRY.
        " a number through a string template: the assignment puts the sign
        " BEHIND it - -5 came out as `5-`, a packed -12.50 as `12.50-`
        IF rtti_check_typekind_number( rtti_get_type_kind( val ) ) = abap_true.
          result = |{ val }|.
        ELSE.
          result = val.
        ENDIF.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    " a number reaches a character target right-aligned in the length its
    " type needs (` 12` for an i on some runtimes), and a CHAR field brings
    " its padding along - neither is what the value says
    result = c_trim( result ).

  ENDMETHOD.

  METHOD check_abap_cloud.

    IF gv_check_cloud_cached = abap_true.
      result = gv_check_cloud.
      RETURN.
    ENDIF.

    TRY.
        cl_abap_typedescr=>describe_by_name( `T100` ).
        gv_check_cloud = abap_false.
      CATCH cx_root.
        gv_check_cloud = abap_true.
    ENDTRY.
    gv_check_cloud_cached = abap_true.
    result = gv_check_cloud.

  ENDMETHOD.

  METHOD sy_get.

    result-datum = sy-datum.
    result-uzeit = sy-uzeit.
    result-uname = sy-uname.
    result-sysid = sy-sysid.
    result-saprl = sy-saprl.

  ENDMETHOD.

  METHOD conv_decode_x_base64.
    DATA lv_web_http_name TYPE c LENGTH 19.
    DATA classname        TYPE c LENGTH 15.
    DATA lx_first         TYPE REF TO cx_root.

    TRY.

        lv_web_http_name = `CL_WEB_HTTP_UTILITY`.
        CALL METHOD (lv_web_http_name)=>(`DECODE_X_BASE64`)
          EXPORTING
            encoded = val
          RECEIVING
            decoded = result.
        RETURN.

      CATCH cx_root INTO lx_first ##NO_HANDLER.
        " the cause is chained below if the classic API fails too - the
        " two-TRY shape of conv_get_string_by_xstring, reasoning there
    ENDTRY.

    TRY.

        classname = `CL_HTTP_UTILITY`.
        CALL METHOD (classname)=>(`DECODE_X_BASE64`)
          EXPORTING
            encoded = val
          RECEIVING
            decoded = result.

      CATCH cx_root.
        RAISE EXCEPTION TYPE z2ui5_cx_popup_error
          EXPORTING
            val      = `UNSUPPORTED_BASE64_API - neither CL_WEB_HTTP_UTILITY nor CL_HTTP_UTILITY could decode here`
            previous = lx_first.
    ENDTRY.

  ENDMETHOD.

  METHOD conv_encode_x_base64.
    DATA lv_web_http_name TYPE c LENGTH 19.
    DATA classname        TYPE c LENGTH 15.
    DATA lx_first         TYPE REF TO cx_root.

    TRY.

        lv_web_http_name = `CL_WEB_HTTP_UTILITY`.
        CALL METHOD (lv_web_http_name)=>(`ENCODE_X_BASE64`)
          EXPORTING
            unencoded = val
          RECEIVING
            encoded   = result.
        RETURN.

      CATCH cx_root INTO lx_first ##NO_HANDLER.
        " see conv_decode_x_base64
    ENDTRY.

    TRY.

        classname = `CL_HTTP_UTILITY`.
        CALL METHOD (classname)=>(`ENCODE_X_BASE64`)
          EXPORTING
            unencoded = val
          RECEIVING
            encoded   = result.

      CATCH cx_root.
        RAISE EXCEPTION TYPE z2ui5_cx_popup_error
          EXPORTING
            val      = `UNSUPPORTED_BASE64_API - neither CL_WEB_HTTP_UTILITY nor CL_HTTP_UTILITY could encode here`
            previous = lx_first.
    ENDTRY.

  ENDMETHOD.

  METHOD conv_get_string_by_xstring.

    DATA lv_invalid TYPE abap_bool.

    " UTF-8 through the code-page method: one implementation of the two
    " conversion APIs and their fallback, not two
    conv_get_string_by_xstring_cp( EXPORTING val      = val
                                             codepage = `UTF-8`
                                             encoding = `UTF-8`
                                   IMPORTING result   = result
                                             invalid  = lv_invalid ).
    IF lv_invalid = abap_true.
      RAISE EXCEPTION TYPE z2ui5_cx_popup_error
        EXPORTING
          val = `INVALID_UTF8 - the bytes are no valid UTF-8`.
    ENDIF.

  ENDMETHOD.

  METHOD conv_get_xstring_by_string.

    DATA conv           TYPE REF TO object.
    DATA conv_codepage  TYPE c LENGTH 21.
    DATA conv_out_class TYPE c LENGTH 19.
    DATA lx_first       TYPE REF TO cx_root.

    TRY.

        conv_codepage = `CL_ABAP_CONV_CODEPAGE`.
        CALL METHOD (conv_codepage)=>create_out
          RECEIVING
            instance = conv.

        CALL METHOD conv->(`IF_ABAP_CONV_OUT~CONVERT`)
          EXPORTING
            source = val
          RECEIVING
            result = result.
        RETURN.

      CATCH cx_root INTO lx_first ##NO_HANDLER.
        " deliberate, same as conv_get_string_by_xstring: the capture is the
        " handling, the fall-through is the point
    ENDTRY.

    " Guarded for the same reason as its counterpart above: an unguarded
    " fallback let a raw dynamic-call exception out of a utility method.
    TRY.

        conv_out_class = `CL_ABAP_CONV_OUT_CE`.
        CALL METHOD (conv_out_class)=>create
          EXPORTING
            encoding = `UTF-8`
          RECEIVING
            conv     = conv.

        CALL METHOD conv->(`CONVERT`)
          EXPORTING
            data   = val
          IMPORTING
            buffer = result.

      CATCH cx_root.
        RAISE EXCEPTION TYPE z2ui5_cx_popup_error
          EXPORTING
            val      = |UNSUPPORTED_CODEPAGE_API - neither CL_ABAP_CONV_CODEPAGE | &&
                       |nor CL_ABAP_CONV_OUT_CE could convert here|
            previous = lx_first.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_get_data_element_texts.

    DATA data_element_name TYPE string.
    DATA lv_do_fallback    TYPE abap_bool.

    data_element_name = val.

    TRY.
        rtti_get_dtel_texts_by_ddic( EXPORTING name        = data_element_name
                                     IMPORTING texts       = result
                                               do_fallback = lv_do_fallback ).
      CATCH cx_root.
        rtti_get_dtel_texts_by_xco( EXPORTING name        = data_element_name
                                    IMPORTING texts       = result
                                              do_fallback = lv_do_fallback ).
    ENDTRY.

    IF lv_do_fallback = abap_true AND result IS INITIAL.
      result-header = val.
      result-long = val.
      result-medium = val.
      result-short = val.
    ENDIF.

  ENDMETHOD.

  METHOD rtti_get_dtel_texts_by_ddic.

    DATA ddic_ref TYPE REF TO data.
    DATA: BEGIN OF ddic,
            reptext   TYPE string,
            scrtext_s TYPE string,
            scrtext_m TYPE string,
            scrtext_l TYPE string,
          END OF ddic.
    DATA struct_descr TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS <ddic> TYPE data.
    DATA lo_typedescr TYPE REF TO cl_abap_typedescr.

    CLEAR texts.
    do_fallback = abap_false.

    cl_abap_typedescr=>describe_by_name( `T100` ).

    struct_descr ?= cl_abap_structdescr=>describe_by_name( `DFIES` ).

    CREATE DATA ddic_ref TYPE HANDLE struct_descr.

    " IS ASSIGNED, not sy-subrc, and a RETURN rather than an ASSERT: the
    " symbol is the unassign_data shape (#1937), and an ASSERTION_FAILED
    " here is a short dump the caller's own TRY cannot take - so the XCO
    " fallback it exists for would never run. The two branches below answer
    " a failure the same way
    ASSIGN ddic_ref->* TO <ddic>.
    IF <ddic> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    cl_abap_elemdescr=>describe_by_name( EXPORTING  p_name     = name
                                         RECEIVING p_descr_ref = lo_typedescr
                                         EXCEPTIONS OTHERS     = 1 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL METHOD lo_typedescr->(`GET_DDIC_FIELD`)
      RECEIVING
        p_flddescr   = <ddic>
      EXCEPTIONS
        not_found    = 1
        no_ddic_type = 2
        OTHERS       = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    MOVE-CORRESPONDING <ddic> TO ddic.
    texts-header = ddic-reptext.
    texts-short  = ddic-scrtext_s.
    texts-medium = ddic-scrtext_m.
    texts-long   = ddic-scrtext_l.
    do_fallback  = abap_true.

  ENDMETHOD.

  METHOD rtti_get_dtel_texts_by_xco.

    DATA data_element TYPE REF TO object.
    DATA content      TYPE REF TO object.
    DATA exists       TYPE abap_bool.
    DATA lv_xco_cp_abap_dictionary TYPE string.
    " A dynamic call checks its actual parameters against the formal ones at
    " runtime and converts nothing, so both sides are typed exactly as XCO
    " declares them. The name is sxco_ad_object_name (CHAR30), passed by
    " reference - a string there raised CX_SY_DYN_CALL_ILLEGAL_TYPE. Each
    " label is if_xco_dtel_content=>ts_field_label, a text (CHAR60) and its
    " length (NUMC2) - received into a string, every one raised the same.
    " Both ended in the CATCH below, so on ABAP Cloud every data element came
    " back with empty texts (or its own name, as the caller's fallback). The
    " shapes are written out because the XCO types exist on cloud only
    DATA lv_name TYPE c LENGTH 30.
    DATA: BEGIN OF ls_label,
            text   TYPE c LENGTH 60,
            length TYPE n LENGTH 2,
          END OF ls_label.

    CLEAR texts.
    do_fallback = abap_false.

    TRY.
        lv_name = name.
        lv_xco_cp_abap_dictionary = `XCO_CP_ABAP_DICTIONARY`.
        CALL METHOD (lv_xco_cp_abap_dictionary)=>(`DATA_ELEMENT`)
          EXPORTING
            iv_name         = lv_name
          RECEIVING
            ro_data_element = data_element.

        CALL METHOD data_element->(`IF_XCO_AD_DATA_ELEMENT~EXISTS`)
          RECEIVING
            rv_exists = exists.

        IF exists = abap_false.
          RETURN.
        ENDIF.

        CALL METHOD data_element->(`IF_XCO_AD_DATA_ELEMENT~CONTENT`)
          RECEIVING
            ro_content = content.

        CALL METHOD content->(`IF_XCO_DTEL_CONTENT~GET_HEADING_FIELD_LABEL`)
          RECEIVING
            rs_heading_field_label = ls_label.
        texts-header = ls_label-text.

        CALL METHOD content->(`IF_XCO_DTEL_CONTENT~GET_SHORT_FIELD_LABEL`)
          RECEIVING
            rs_short_field_label = ls_label.
        texts-short = ls_label-text.

        CALL METHOD content->(`IF_XCO_DTEL_CONTENT~GET_MEDIUM_FIELD_LABEL`)
          RECEIVING
            rs_medium_field_label = ls_label.
        texts-medium = ls_label-text.

        CALL METHOD content->(`IF_XCO_DTEL_CONTENT~GET_LONG_FIELD_LABEL`)
          RECEIVING
            rs_long_field_label = ls_label.
        texts-long = ls_label-text.

        do_fallback = abap_true.

      CATCH cx_root.
        do_fallback = abap_true.
    ENDTRY.

  ENDMETHOD.

  METHOD uuid_get_c32.
    DATA lv_uuid      TYPE c LENGTH 32.
    DATA lv_classname TYPE string.
    DATA lv_fm        TYPE string.
        DATA lx_uuid TYPE REF TO cx_root.

    TRY.

        TRY.

            lv_classname = `CL_SYSTEM_UUID`.
            CALL METHOD (lv_classname)=>if_system_uuid_static~create_uuid_c32
              RECEIVING
                uuid = lv_uuid.

          CATCH cx_root.

            lv_fm = `GUID_CREATE`.
            " error_message: a message the module sends would otherwise end
            " the request, which no CATCH reaches
            CALL FUNCTION lv_fm
              IMPORTING
                ev_guid_32    = lv_uuid
              EXCEPTIONS
                error_message = 1
                OTHERS        = 2.
            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE z2ui5_cx_popup_error
                EXPORTING
                  val = `GUID_CREATE_FAILED`.
            ENDIF.

        ENDTRY.

        result = lv_uuid.


      CATCH cx_root INTO lx_uuid.
        " both UUID mechanisms failed - raise the framework exception so the
        " consumer's single top-level catch can turn it into a handled error.
        " ASSERT would raise the uncatchable ASSERTION_FAILED and bypass that
        " catch (short dump instead of a handled error response).
        " z2ui5_cx_popup_error reads a uuid when it renders its full report,
        " so a renderer running while the raise below is on its way would
        " re-enter this method. gv_uuid_failed makes that nested call return an
        " empty UUID instead of raising again.
        IF gv_uuid_failed = abap_true.
          RETURN.
        ENDIF.
        gv_uuid_failed = abap_true.
        TRY.
            RAISE EXCEPTION TYPE z2ui5_cx_popup_error
              EXPORTING
                val = lx_uuid.
          CLEANUP.
            CLEAR gv_uuid_failed.
        ENDTRY.
    ENDTRY.
  ENDMETHOD.

  METHOD msg_get_internal.
        FIELD-SYMBOLS <tab> TYPE ANY TABLE.
        DATA lt_attri TYPE abap_component_tab.
        DATA temp44 TYPE ty_s_msg.
        DATA ls_result LIKE temp44.
        DATA temp45 LIKE LINE OF lt_attri.
        DATA ls_attri LIKE REF TO temp45.
          FIELD-SYMBOLS <comp> TYPE any.
          DATA temp46 TYPE z2ui5_cl_popup_context=>ty_s_msg.

    CASE rtti_get_type_kind( val ).

      WHEN cl_abap_datadescr=>typekind_table.

        " guarded like data_render_tab and data_get_headline - handing an
        " UNASSIGNED symbol to the method below is a GETWA_NOT_ASSIGNED
        " dump. No messages then, which is what "nothing in here is a
        " message" means to the caller: it falls back to the data renderer
        ASSIGN val TO <tab>.
        IF <tab> IS NOT ASSIGNED.
          RETURN.
        ENDIF.
        result = msg_get_internal_tab( <tab> ).

      WHEN cl_abap_datadescr=>typekind_struct1 OR cl_abap_datadescr=>typekind_struct2.

        IF val IS INITIAL.
          RETURN.
        ENDIF.

        IF iv_check_rap = abap_true AND check_is_rap_struct( val ) = abap_true.
          result = msg_get_rap( val ).
          RETURN.
        ENDIF.


        lt_attri = rtti_get_t_attri_by_any( val ).


        CLEAR temp44.

        ls_result = temp44.


        LOOP AT lt_attri REFERENCE INTO ls_attri.

          ASSIGN COMPONENT ls_attri->name OF STRUCTURE val TO <comp>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          " only an ITEM that can HOLD messages is the envelope's payload -
          " a table, a structure or an object (the BALI item). A plain
          " ITEM field (a position number) became the whole message and
          " dropped the structure's other fields
          IF ls_attri->name = `ITEM` AND check_msg_container( <comp> ) = abap_true.
            result = msg_get_internal( <comp> ).
            RETURN.
          ENDIF.
          ls_result = msg_map( name = ls_attri->name
                               val  = <comp>
                               msg  = ls_result ).

        ENDLOOP.
        " a T100 message is a class AND a number. A component called ID
        " alone is the key column of any business row - built into a
        " message, it gave `I:0001:` (a missing class still answers a text)
        " and the data never reached the data renderer it was meant for
        IF ls_result-text IS INITIAL AND ls_result-id IS NOT INITIAL AND ls_result-no IS NOT INITIAL.
          ls_result-id = to_upper( ls_result-id ).
          MESSAGE ID ls_result-id TYPE `I` NUMBER ls_result-no
                  WITH ls_result-v1 ls_result-v2 ls_result-v3 ls_result-v4
                  INTO ls_result-text.
        ENDIF.
        INSERT ls_result INTO TABLE result.

      WHEN cl_abap_datadescr=>typekind_oref.
        " an unbound reference carries no message - guarded like the struct
        " and table branches guard theirs. Unguarded it fell through the
        " three handlers of msg_get_by_oref into a describe on the null
        " reference, and message_box_display( unbound ) ended in a 500
        " instead of the documented silence
        IF val IS NOT INITIAL.
          result = msg_get_by_oref( val ).
        ENDIF.

      WHEN OTHERS.

        " skip an empty character value like the struct branch does -
        " otherwise msg_get_t's val2 fallback can never take over and the
        " caller renders a message box with blank text
        IF rtti_check_clike( val ) IS NOT INITIAL AND val IS NOT INITIAL.

          CLEAR temp46.
          temp46-text = val.
          INSERT temp46 INTO TABLE result.
        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD msg_get_by_oref.
        DATA temp47 TYPE REF TO cx_root.
        DATA lx LIKE temp47.
        DATA temp48 TYPE z2ui5_cl_popup_context=>ty_s_msg.
    DATA obj TYPE REF TO object.
    DATA lr_tab TYPE REF TO data.
    FIELD-SYMBOLS <tab> TYPE any.
    DATA temp49 TYPE z2ui5_cl_popup_context=>ty_s_msg.

    " four shapes, tried in turn and each on its own level - the nested
    " TRY/CATCH pyramid this used to be carried the attribute mapping twice,
    " three CATCH blocks deep, one fix away from drifting. Every attempt
    " that fails falls through to the next; the last one cannot fail
    " quietly, it maps whatever public attributes the object has

    " 1. an exception: its text, and its public attributes over it
    TRY.

        temp47 ?= val.

        lx = temp47.

        CLEAR temp48.
        temp48-type = `E`.
        temp48-text = lx->get_text( ).
        INSERT msg_get_by_oref_attri( io_obj = lx
                                      is_msg = temp48 ) INTO TABLE result.
        RETURN.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.


    obj = val.



    " 2. an application log (IF_BALI_LOG)
    TRY.
        CREATE DATA lr_tab TYPE (`if_bali_log=>ty_item_table`).
        ASSIGN lr_tab->* TO <tab>.
        CALL METHOD obj->(`IF_BALI_LOG~GET_ALL_ITEMS`)
          RECEIVING
            item_table = <tab>.
        INSERT LINES OF msg_get_internal( <tab> ) INTO TABLE result.
        RETURN.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    " 3. a logger (ZIF_LOGGER)
    TRY.
        CREATE DATA lr_tab TYPE (`BAPIRETTAB`).
        ASSIGN lr_tab->* TO <tab>.
        CALL METHOD obj->(`ZIF_LOGGER~EXPORT_TO_TABLE`)
          RECEIVING
            rt_bapiret = <tab>.
        INSERT LINES OF msg_get_internal( <tab> ) INTO TABLE result.
        RETURN.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    " 4. any object: whatever public attributes carry a message part

    CLEAR temp49.
    INSERT msg_get_by_oref_attri( io_obj = obj
                                  is_msg = temp49 ) INTO TABLE result.

  ENDMETHOD.

  METHOD msg_map.

    result = msg.

    " The value is a component of an ARBITRARY structure: msg_get_internal
    " walks whatever an app hands to message_box_display( ) and maps every
    " component BY NAME. A business structure with a component that happens
    " to be called TEXT, ID, TYPE or V1 and is a TABLE, a nested structure
    " or a reference is not a message part - and the assignments below are
    " no class-based exception for it, they are a MOVE type conflict
    " (OBJECTS_MOVE_NOT_SUPPORTED on a system, "table, no header line" in
    " the transpiled runtime). So the box that was supposed to REPORT a
    " problem was the crash: nothing between here and the app's own main( )
    " catches it, and the roundtrip ended as a 500.
    " Decided BEFORE anything is assigned, which is the same rule
    " z2ui5_cl_ui5_srv_model=>delta_apply_field follows for the same reason
    " - a runtime error cannot be caught after the fact. A skipped component
    " simply is not a message part: the entry ends up without a text,
    " ui5_msg_box_format drops it, and the caller falls back to the DATA
    " renderer, which shows the whole structure including that component
    IF rtti_check_printable( val ) = abap_false.
      RETURN.
    ENDIF.

    CASE name.
      WHEN `ID` OR `MSGID` OR `ARBGB`.
        result-id = val.
      WHEN `NO` OR `NUMBER` OR `MSGNO` OR `TXTNR`.
        result-no = val.
      WHEN `MESSAGE` OR `TEXT`.
        result-text = val.
      WHEN `TYPE` OR `MSGTY` OR `M_SEVERITY`.
        result-type = val.
      WHEN `MESSAGE_V1` OR `MSGV1` OR `V1`.
        result-v1 = val.
      WHEN `MESSAGE_V2` OR `MSGV2` OR `V2`.
        result-v2 = val.
      WHEN `MESSAGE_V3` OR `MSGV3` OR `V3`.
        result-v3 = val.
      WHEN `MESSAGE_V4` OR `MSGV4` OR `V4`.
        result-v4 = val.
      WHEN `TIME_STMP`.
        " the one target that is not a string: a printable but non-numeric
        " value (`2026-09-16 10:00` in a char field of that name) converts
        " into the packed timestamp with CX_SY_CONVERSION_NO_NUMBER, which
        " is catchable where the MOVE conflict above is not - caught, and
        " the entry keeps no timestamp rather than costing the whole box
        TRY.
            result-timestampl = val.
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.
    ENDCASE.

  ENDMETHOD.

  METHOD check_is_rap_struct.

    DATA lt_attri TYPE abap_component_tab.
    DATA temp50 LIKE LINE OF lt_attri.
    DATA ls_attri LIKE REF TO temp50.
          DATA temp51 TYPE REF TO cl_abap_tabledescr.
          DATA lo_line TYPE REF TO cl_abap_datadescr.
          DATA temp52 TYPE REF TO cl_abap_structdescr.
          DATA lt_comps TYPE abap_component_tab.
          DATA temp53 LIKE LINE OF lt_comps.
          DATA ls_comp LIKE REF TO temp53.
    lt_attri = rtti_get_t_attri_by_any( val ).



    LOOP AT lt_attri REFERENCE INTO ls_attri.
      CASE ls_attri->name.
        WHEN `%MSG` OR `%FAIL` OR `%OTHER`.
          result = abap_true.
          RETURN.
      ENDCASE.
    ENDLOOP.

    LOOP AT lt_attri REFERENCE INTO ls_attri.
      " the component's descriptor is already in the attribute table - no
      " ASSIGN and no describe_by_data per component
      IF ls_attri->type->kind <> cl_abap_typedescr=>kind_table.
        CONTINUE.
      ENDIF.

      TRY.

          temp51 ?= ls_attri->type.

          lo_line = temp51->get_table_line_type( ).
          IF lo_line->kind <> cl_abap_typedescr=>kind_struct.
            CONTINUE.
          ENDIF.

          temp52 ?= lo_line.

          lt_comps = temp52->get_components( ).


          LOOP AT lt_comps REFERENCE INTO ls_comp.
            IF ls_comp->name = `%MSG` OR ls_comp->name = `%FAIL`.
              result = abap_true.
              RETURN.
            ENDIF.
          ENDLOOP.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD msg_get_rap.

    DATA lv_kind TYPE string.
    DATA lv_is_row TYPE abap_bool.
    DATA lt_attri TYPE abap_component_tab.
    DATA temp54 LIKE LINE OF lt_attri.
    DATA ls_attri LIKE REF TO temp54.
      FIELD-SYMBOLS <tab> TYPE any.
      FIELD-SYMBOLS <ftab> TYPE ANY TABLE.
      DATA temp55 TYPE REF TO cl_abap_tabledescr.
      DATA lo_line TYPE REF TO cl_abap_datadescr.
      FIELD-SYMBOLS <row> TYPE ANY.
    lv_kind = rtti_get_type_kind( val ).
    IF lv_kind <> cl_abap_datadescr=>typekind_struct1
        AND lv_kind <> cl_abap_datadescr=>typekind_struct2.
      RETURN.
    ENDIF.


    msg_get_rap_row( EXPORTING val         = val
                               entity_name = entity_name
                     IMPORTING messages    = result
                               is_row      = lv_is_row ).
    IF lv_is_row = abap_true.
      RETURN.
    ENDIF.


    lt_attri = rtti_get_t_attri_by_any( val ).


    LOOP AT lt_attri REFERENCE INTO ls_attri.

      ASSIGN COMPONENT ls_attri->name OF STRUCTURE val TO <tab>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      IF rtti_get_type_kind( <tab> ) <> cl_abap_datadescr=>typekind_table.
        CONTINUE.
      ENDIF.

      " guarded like data_render_tab, msg_get_internal and
      " data_get_headline - and here the symbol is LOOP-CARRIED, so an
      " ASSIGN that fails leaves it on the PREVIOUS component's table and
      " the rows below would be read out of that one

      UNASSIGN <ftab>.
      ASSIGN <tab> TO <ftab>.
      IF <ftab> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      " the row kind is a property of the table TYPE - decided once per
      " table, not asked of every row (same idea as msg_get_internal_tab)

      temp55 ?= ls_attri->type.

      lo_line = temp55->get_table_line_type( ).

      LOOP AT <ftab> ASSIGNING <row>.
        IF lo_line->type_kind = cl_abap_datadescr=>typekind_oref.
          IF <row> IS NOT INITIAL.
            TRY.
                INSERT LINES OF msg_get_t( <row> ) INTO TABLE result.
              CATCH cx_root ##NO_HANDLER.
            ENDTRY.
          ENDIF.
        ELSE.
          INSERT LINES OF msg_get_rap( val         = <row>
                                       entity_name = ls_attri->name ) INTO TABLE result.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD msg_get_rap_row.
    DATA lv_meta_built TYPE abap_bool.
    DATA lt_meta TYPE ty_t_name_value.
    FIELD-SYMBOLS <msg> TYPE any.
            DATA lt_one TYPE z2ui5_cl_popup_context=>ty_t_msg.
            FIELD-SYMBOLS <m> LIKE LINE OF lt_one.
    FIELD-SYMBOLS <fail> TYPE any.
      FIELD-SYMBOLS <cause> TYPE any.
        DATA lv_cause TYPE i.
        DATA lv_text TYPE string.
        DATA temp56 TYPE z2ui5_cl_popup_context=>ty_s_msg.

    CLEAR messages.
    is_row = abap_false.

    " the meta block is built LAZILY: msg_get_rap_meta walks the row's
    " components three times over (element/action/tky scans), and the
    " common row in a RAP response table carries neither a filled %MSG nor
    " a %FAIL - building the block up front threw that work away per row




    ASSIGN COMPONENT `%MSG` OF STRUCTURE val TO <msg>.
    IF sy-subrc = 0.
      is_row = abap_true.
      IF <msg> IS NOT INITIAL.
        lt_meta = msg_get_rap_meta( val ).
        lv_meta_built = abap_true.
        TRY.

            lt_one = msg_get_t( <msg> ).

            LOOP AT lt_one ASSIGNING <m>.
              <m>-t_meta = lt_meta.
            ENDLOOP.
            INSERT LINES OF lt_one INTO TABLE messages.
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.
      ENDIF.
    ENDIF.


    ASSIGN COMPONENT `%FAIL` OF STRUCTURE val TO <fail>.
    IF sy-subrc = 0.
      is_row = abap_true.

      ASSIGN COMPONENT `CAUSE` OF STRUCTURE <fail> TO <cause>.
      IF sy-subrc = 0.
        IF lv_meta_built = abap_false.
          lt_meta = msg_get_rap_meta( val ).
        ENDIF.
        " %FAIL-CAUSE is the enumeration IF_ABAP_BEHV=>T_FAIL_CAUSE (base
        " type i). An enumerated value does not convert to i - a plain MOVE
        " is the runtime error UC_OBJECTS_NOT_CONVERTIBLE - so it is read by
        " its name (a string template writes the name) and mapped to the
        " base value. Type kind `k` is the enumeration (literal: the
        " constant typekind_enum does not exist on 7.02).

        IF cl_abap_typedescr=>describe_by_data( <cause> )->type_kind = `k`.
          lv_cause = msg_get_rap_cause_value( |{ <cause> }| ).
        ELSE.
          lv_cause = <cause>.
        ENDIF.

        lv_text = msg_get_rap_fail_text( lv_cause ).
        IF entity_name IS NOT INITIAL.
          lv_text = |{ entity_name }: { lv_text }|.
        ENDIF.

        CLEAR temp56.
        temp56-type = `E`.
        temp56-text = lv_text.
        temp56-t_meta = lt_meta.
        INSERT temp56 INTO TABLE messages.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_comp_str.

    " printable first, then the assignment: the component comes out of a
    " structure this class did not declare, and a table or a nested
    " structure into a string is a MOVE type conflict, not an exception -
    " see msg_map, which learnt it the expensive way.
    " One chained AND and not the nested IFs z2ui5_cl_ui5_handler=>
    " request_context_info argues for: abaplint's if_in_if refuses the
    " nesting here (nothing sits between the two tests), and the second
    " operand is safe on a failed ASSIGN either way - ABAP evaluates a
    " logical expression left to right and stops once the result is decided
    FIELD-SYMBOLS <comp> TYPE any.
    ASSIGN COMPONENT comp OF STRUCTURE val TO <comp>.
    IF sy-subrc = 0 AND rtti_check_printable( <comp> ) = abap_true.
      result = <comp>.
    ENDIF.

  ENDMETHOD.

  METHOD scan_flag_prefix.

    DATA lv_kind TYPE string.
    DATA temp57 TYPE string.
    DATA lv_prefix LIKE temp57.
    DATA lv_off TYPE i.
      FIELD-SYMBOLS <sub> TYPE any.
    DATA lv_len TYPE i.
    DATA lt_attri TYPE abap_component_tab.
    DATA temp58 LIKE LINE OF lt_attri.
    DATA ls_attri LIKE REF TO temp58.
      FIELD-SYMBOLS <flag> TYPE any.
    lv_kind = rtti_get_type_kind( val ).
    IF lv_kind <> cl_abap_datadescr=>typekind_struct1
        AND lv_kind <> cl_abap_datadescr=>typekind_struct2.
      RETURN.
    ENDIF.

    " a `-` in the prefix is a path, not part of a component name - no
    " component name can contain one: %ELEMENT-<field> and
    " %OP-%ACTION-<name> are components of the NESTED structures %ELEMENT
    " and %OP-%ACTION. Compared against the row's own component names, the
    " two RAP prefixes never matched, and no message ever carried its
    " element or action

    temp57 = prefix.

    lv_prefix = temp57.

    lv_off = find( val = lv_prefix
                         sub = `-` ).
    IF lv_off >= 0.

      ASSIGN COMPONENT lv_prefix(lv_off) OF STRUCTURE val TO <sub>.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      result = scan_flag_prefix( val    = <sub>
                                 prefix = substring( val = lv_prefix
                                                     off = lv_off + 1 ) ).
      RETURN.
    ENDIF.


    lv_len = strlen( lv_prefix ).

    lt_attri = rtti_get_t_attri_by_any( val ).


    LOOP AT lt_attri REFERENCE INTO ls_attri.
      IF strlen( ls_attri->name ) <= lv_len.
        CONTINUE.
      ENDIF.
      IF NOT ( lv_len = 0 OR ls_attri->name(lv_len) = lv_prefix ).
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT ls_attri->name OF STRUCTURE val TO <flag>.
      IF sy-subrc <> 0 OR <flag> IS INITIAL.
        CONTINUE.
      ENDIF.
      INSERT ls_attri->name+lv_len INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD msg_get_rap_element.

    DATA lt_suffix TYPE string_table.
    lt_suffix = scan_flag_prefix( val       = val
                                        prefix = `%ELEMENT-` ).
    result = concat_lines_of( table = lt_suffix
                              sep   = `, ` ).

  ENDMETHOD.

  METHOD msg_get_rap_state_area.

    result = get_comp_str( val     = val
                           comp = `%STATE_AREA` ).

  ENDMETHOD.

  METHOD msg_get_rap_action.

    DATA lt_suffix TYPE string_table.
    DATA temp59 TYPE string.
    DATA temp60 TYPE string.
    lt_suffix = scan_flag_prefix( val       = val
                                        prefix = `%OP-%ACTION-` ).

    CLEAR temp59.

    READ TABLE lt_suffix INTO temp60 INDEX 1.
    IF sy-subrc = 0.
      temp59 = temp60.
    ENDIF.
    result = temp59.

  ENDMETHOD.

  METHOD msg_get_rap_pid.

    result = get_comp_str( val     = val
                           comp = `%PID` ).

  ENDMETHOD.

  METHOD msg_get_rap_cid.

    result = get_comp_str( val     = val
                           comp = `%CID` ).

  ENDMETHOD.

  METHOD msg_get_rap_tky.

    FIELD-SYMBOLS <tky> TYPE any.
    ASSIGN COMPONENT `%TKY` OF STRUCTURE val TO <tky>.
    IF sy-subrc <> 0 OR <tky> IS INITIAL.
      RETURN.
    ENDIF.
    result = msg_get_rap_flatten( <tky> ).

  ENDMETHOD.

  METHOD msg_get_rap_flatten.

    DATA lv_kind TYPE string.
    DATA lt_attri TYPE abap_component_tab.
    DATA lt_part TYPE string_table.
    DATA temp61 LIKE LINE OF lt_attri.
    DATA ls_attri LIKE REF TO temp61.
      FIELD-SYMBOLS <comp> TYPE any.
      DATA lv_sub_kind TYPE string.
        DATA lv_sub TYPE string.
            DATA lv_str TYPE string.
            DATA temp62 LIKE LINE OF lt_part.
    lv_kind = rtti_get_type_kind( val ).
    IF lv_kind <> cl_abap_datadescr=>typekind_struct1
        AND lv_kind <> cl_abap_datadescr=>typekind_struct2.
      RETURN.
    ENDIF.


    lt_attri = rtti_get_t_attri_by_any( val ).




    LOOP AT lt_attri REFERENCE INTO ls_attri.

      ASSIGN COMPONENT ls_attri->name OF STRUCTURE val TO <comp>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.


      lv_sub_kind = rtti_get_type_kind( <comp> ).
      IF lv_sub_kind = cl_abap_datadescr=>typekind_struct1
          OR lv_sub_kind = cl_abap_datadescr=>typekind_struct2.

        lv_sub = msg_get_rap_flatten( <comp> ).
        IF lv_sub IS NOT INITIAL.
          INSERT lv_sub INTO TABLE lt_part.
        ENDIF.
      ELSEIF <comp> IS NOT INITIAL AND rtti_check_printable( <comp> ) = abap_true.
        " the printable test does what the TRY around this could not: a
        " TABLE component into the string below is a MOVE type conflict and
        " therefore a runtime error, which no CATCH takes (see msg_map). The
        " TRY stays for the conversions that ARE class-based - a character
        " value the packed/numeric target refuses
        TRY.

            lv_str = <comp>.

            temp62 = |{ ls_attri->name }={ lv_str }|.
            INSERT temp62 INTO TABLE lt_part.
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.
      ENDIF.
    ENDLOOP.

    result = concat_lines_of( table = lt_part
                              sep   = `, ` ).

  ENDMETHOD.

  METHOD msg_get_rap_meta.

    DATA lv TYPE string.
      DATA temp63 TYPE z2ui5_cl_popup_context=>ty_s_name_value.
      DATA temp64 TYPE z2ui5_cl_popup_context=>ty_s_name_value.
      DATA temp65 TYPE z2ui5_cl_popup_context=>ty_s_name_value.
      DATA temp66 TYPE z2ui5_cl_popup_context=>ty_s_name_value.
      DATA temp67 TYPE z2ui5_cl_popup_context=>ty_s_name_value.
      DATA temp68 TYPE z2ui5_cl_popup_context=>ty_s_name_value.

    lv = msg_get_rap_element( val ).
    IF lv IS NOT INITIAL.

      CLEAR temp63.
      temp63-n = `element`.
      temp63-v = lv.
      INSERT temp63 INTO TABLE result.
    ENDIF.

    lv = msg_get_rap_state_area( val ).
    IF lv IS NOT INITIAL.

      CLEAR temp64.
      temp64-n = `state_area`.
      temp64-v = lv.
      INSERT temp64 INTO TABLE result.
    ENDIF.

    lv = msg_get_rap_action( val ).
    IF lv IS NOT INITIAL.

      CLEAR temp65.
      temp65-n = `action`.
      temp65-v = lv.
      INSERT temp65 INTO TABLE result.
    ENDIF.

    lv = msg_get_rap_pid( val ).
    IF lv IS NOT INITIAL.

      CLEAR temp66.
      temp66-n = `pid`.
      temp66-v = lv.
      INSERT temp66 INTO TABLE result.
    ENDIF.

    lv = msg_get_rap_cid( val ).
    IF lv IS NOT INITIAL.

      CLEAR temp67.
      temp67-n = `cid`.
      temp67-v = lv.
      INSERT temp67 INTO TABLE result.
    ENDIF.

    lv = msg_get_rap_tky( val ).
    IF lv IS NOT INITIAL.

      CLEAR temp68.
      temp68-n = `tky`.
      temp68-v = lv.
      INSERT temp68 INTO TABLE result.
    ENDIF.

  ENDMETHOD.

  METHOD msg_get_rap_fail_text.

    " the base values of IF_ABAP_BEHV=>T_FAIL_CAUSE, read on an S/4HANA
    " system (2026-11-13) - HTTP-like codes, not a running number
    DATA temp69 TYPE string.
    CASE cause.
      WHEN 0.
        temp69 = `Operation failed`.
      WHEN 401.
        temp69 = `Authorization failure`.
      WHEN 404.
        temp69 = `Entity not found`.
      WHEN 409.
        temp69 = `Concurrent modification`.
      WHEN 423.
        temp69 = `Entity is locked`.
      WHEN 424.
        temp69 = `Dependent operation failed`.
      WHEN 501.
        temp69 = `Operation not implemented`.
      WHEN 4221.
        temp69 = `Operation disabled`.
      WHEN 4222.
        temp69 = `Entity is read-only`.
      WHEN OTHERS.
        temp69 = |Operation failed (cause code { cause })|.
    ENDCASE.
    result = temp69.

  ENDMETHOD.

  METHOD msg_get_rap_cause_value.

    " the name of an IF_ABAP_BEHV=>T_FAIL_CAUSE value to its base value;
    " -1 for a name this release does not know, which the text keeps
    " (the upper-case name in a variable first: the downport turns the
    " SWITCH into a CASE, whose operand takes no built-in function on 7.02)
    DATA lv_name TYPE string.
    DATA temp70 TYPE i.
    lv_name = to_upper( name ).

    CASE lv_name.
      WHEN `UNSPECIFIC`.
        temp70 = 0.
      WHEN `UNAUTHORIZED`.
        temp70 = 401.
      WHEN `NOT_FOUND`.
        temp70 = 404.
      WHEN `CONFLICT`.
        temp70 = 409.
      WHEN `LOCKED`.
        temp70 = 423.
      WHEN `DEPENDENCY`.
        temp70 = 424.
      WHEN `NOT_IMPLEMENTED`.
        temp70 = 501.
      WHEN `DISABLED`.
        temp70 = 4221.
      WHEN `READONLY`.
        temp70 = 4222.
      WHEN OTHERS.
        temp70 = -1.
    ENDCASE.
    result = temp70.

  ENDMETHOD.

  METHOD conv_get_string_by_xstring_cp.

    DATA conv          TYPE REF TO object.
    DATA conv_codepage TYPE c LENGTH 21.
    DATA conv_in_class TYPE c LENGTH 18.
    DATA lv_encoding   TYPE c LENGTH 20.
    DATA lx_first      TYPE REF TO cx_root.

    CLEAR result.
    invalid = abap_false.

    TRY.

        conv_codepage = `CL_ABAP_CONV_CODEPAGE`.
        CALL METHOD (conv_codepage)=>create_in
          EXPORTING
            codepage = codepage
          RECEIVING
            instance = conv.

        CALL METHOD conv->(`IF_ABAP_CONV_IN~CONVERT`)
          EXPORTING
            source = val
          RECEIVING
            result = result.
        RETURN.

      CATCH cx_sy_conversion_codepage.
        " the API is there and the bytes are not valid in this code page -
        " the classic API would reject them too
        CLEAR result.
        invalid = abap_true.
        RETURN.

      CATCH cx_root INTO lx_first ##NO_HANDLER.
        " deliberate: capturing the cause IS the handling - it is chained into
        " UNSUPPORTED_CODEPAGE_API below if the classic API fails too. Falling
        " through is the point; the modern class is absent on older releases,
        " which is the whole reason both paths exist. A code page the modern
        " class does not know (CX_PARAMETER_INVALID_RANGE) falls through too
    ENDTRY.

    " The classic API. Guarded by its own TRY since 2026-09: it used to be the
    " BODY of the CATCH above, so when it failed too the dynamic-call
    " exception left this method raw. On a release that has neither class -
    " and in any runtime that is not an SAP system, where a dynamic
    " CALL METHOD resolves nothing - that reached the caller as
    " CX_SY_DYN_CALL_ILLEGAL_CLASS, which nothing here catches by name.
    " The view builder is one of those callers and builds its
    " control-character set through this method, so a missing codepage class
    " took down every view render with an exception that named a kernel
    " class rather than the framework.
    TRY.

        conv_in_class = `CL_ABAP_CONV_IN_CE`.
        lv_encoding = encoding.
        CALL METHOD (conv_in_class)=>create
          EXPORTING
            encoding = lv_encoding
          RECEIVING
            conv     = conv.

        CALL METHOD conv->(`CONVERT`)
          EXPORTING
            input = val
          IMPORTING
            data  = result.

      CATCH cx_sy_conversion_codepage.
        CLEAR result.
        invalid = abap_true.

      CATCH cx_root.
        RAISE EXCEPTION TYPE z2ui5_cx_popup_error
          EXPORTING
            val      = |UNSUPPORTED_CODEPAGE_API - neither CL_ABAP_CONV_CODEPAGE | &&
                       |nor CL_ABAP_CONV_IN_CE could convert here|
            previous = lx_first.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_get_t_attri_on_prem.

    DATA structdescr TYPE REF TO cl_abap_structdescr.
    DATA dfies       TYPE REF TO data.
    DATA s_dfies     TYPE ty_s_dfies.

    FIELD-SYMBOLS <dfies> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line>  TYPE any.

    DATA temp9           TYPE cl_abap_structdescr=>component_table.
    DATA comps           LIKE temp9.
    DATA temp10          TYPE REF TO cl_abap_structdescr.
    DATA lo_struct       LIKE temp10.
    DATA new_struct_desc TYPE REF TO cl_abap_structdescr.
    DATA new_table_desc  TYPE REF TO cl_abap_tabledescr.
    DATA comp            LIKE LINE OF comps.
    FIELD-SYMBOLS <value>      TYPE any.
    FIELD-SYMBOLS <value_dest> TYPE any.

    comps = temp9.

    temp10 ?= cl_abap_structdescr=>describe_by_name( `DFIES` ).

    lo_struct = temp10.
    comps = lo_struct->get_components( ).

    TRY.

        new_struct_desc = cl_abap_structdescr=>create( comps ).

        new_table_desc = cl_abap_tabledescr=>create( p_line_type  = new_struct_desc
                                                     p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA dfies TYPE HANDLE new_table_desc.

        ASSIGN dfies->* TO <dfies>.
        IF <dfies> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        IF tabname IS INITIAL.
          RAISE EXCEPTION TYPE z2ui5_cx_popup_error
            EXPORTING
              val = `RTTI_BY_NAME_TAB_INITIAL`.
        ENDIF.

        structdescr ?= cl_abap_structdescr=>describe_by_name( tabname ).
        <dfies> = structdescr->get_ddic_field_list( ).

        LOOP AT <dfies> ASSIGNING <line>.

          LOOP AT comps INTO comp.

            UNASSIGN <value>.
            ASSIGN COMPONENT comp-name OF STRUCTURE <line> TO <value>.
            IF <value> IS NOT ASSIGNED.
              CONTINUE.
            ENDIF.

            UNASSIGN <value_dest>.
            ASSIGN COMPONENT comp-name OF STRUCTURE s_dfies TO <value_dest>.
            IF <value_dest> IS NOT ASSIGNED.
              CONTINUE.
            ENDIF.

            <value_dest> = <value>.

            UNASSIGN <value>.
            UNASSIGN <value_dest>.

          ENDLOOP.

          INSERT s_dfies INTO TABLE result.
          CLEAR s_dfies.

        ENDLOOP.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_get_t_attri_on_cloud.

    DATA obj TYPE REF TO object.
    DATA lv_tabname TYPE c LENGTH 16.
    DATA lr_ddfields TYPE REF TO data.
    TYPES ty_c30 TYPE c LENGTH 30.
    TYPES temp4 TYPE STANDARD TABLE OF ty_c30 WITH DEFAULT KEY.
DATA names TYPE temp4.
    FIELD-SYMBOLS <any> TYPE any.
    FIELD-SYMBOLS <field> TYPE simple.
    FIELD-SYMBOLS <ddfields> TYPE ANY TABLE.
            DATA lv_method2 TYPE string.
            DATA workaround TYPE string.
            DATA temp71 TYPE REF TO cl_abap_structdescr.
    DATA lt_comp TYPE abap_component_tab.
    DATA temp72 LIKE LINE OF lt_comp.
    DATA lr_comp LIKE REF TO temp72.
      DATA lv_check_key LIKE abap_false.
      DATA temp73 LIKE sy-subrc.
      DATA temp74 TYPE z2ui5_cl_popup_context=>ty_s_dfies.

* convert to correct type,
    lv_tabname = tabname.

    TRY.
        TRY.

            lv_method2 = `XCO_CP_ABAP_DICTIONARY`.
            CALL METHOD (lv_method2)=>(`DATABASE_TABLE`)
              EXPORTING
                iv_name           = lv_tabname
              RECEIVING
                ro_database_table = obj.
            ASSIGN obj->(`IF_XCO_DATABASE_TABLE~FIELDS->IF_XCO_DBT_FIELDS_FACTORY~KEY`) TO <any>.
            IF sy-subrc  <> 0.
* fallback to RTTI, KEY field does not exist in S/4 2020
              RAISE EXCEPTION TYPE cx_sy_dyn_call_illegal_class.
            ENDIF.
            obj = <any>.
            CALL METHOD obj->(`IF_XCO_DBT_FIELDS~GET_NAMES`)
              RECEIVING
                rt_names = names.
          CATCH cx_sy_dyn_call_illegal_class.

            workaround = `DDFIELDS`.
            CREATE DATA lr_ddfields TYPE (workaround).
            ASSIGN lr_ddfields->* TO <ddfields>.
            ASSERT sy-subrc = 0.

            temp71 ?= cl_abap_typedescr=>describe_by_name( lv_tabname ).
            <ddfields> = temp71->get_ddic_field_list( ).
            LOOP AT <ddfields> ASSIGNING <any>.
              ASSIGN COMPONENT `KEYFLAG` OF STRUCTURE <any> TO <field>.
              IF sy-subrc <> 0 OR <field> <> abap_true.
                CONTINUE.
              ENDIF.
              ASSIGN COMPONENT `FIELDNAME` OF STRUCTURE <any> TO <field>.
              ASSERT sy-subrc = 0.
              INSERT <field> INTO TABLE names.
            ENDLOOP.
        ENDTRY.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.


    " by NAME: rtti_get_t_attri_by_any( tabname ) described the string
    " holding the name, and the cast to a structure failed every time

    lt_comp = rtti_get_t_attri_by_table_name( tabname ).


    LOOP AT lt_comp REFERENCE INTO lr_comp.


      lv_check_key = abap_false.

      READ TABLE names WITH KEY table_line = lr_comp->name TRANSPORTING NO FIELDS.
      temp73 = sy-subrc.
      IF temp73 = 0.
        lv_check_key = abap_true.
      ENDIF.


      CLEAR temp74.
      temp74-fieldname = lr_comp->name.
      temp74-rollname = lr_comp->name.
      temp74-keyflag = lv_check_key.
      temp74-scrtext_s = lr_comp->name.
      temp74-scrtext_m = lr_comp->name.
      temp74-scrtext_l = lr_comp->name.
      INSERT temp74 INTO TABLE result.

    ENDLOOP.

  ENDMETHOD.

  METHOD rtti_get_t_dfies_by_table_name.

    IF check_abap_cloud( ) IS NOT INITIAL.
      result = rtti_get_t_attri_on_cloud( table_name ).
    ELSE.
      result = rtti_get_t_attri_on_prem( table_name ).
    ENDIF.

  ENDMETHOD.

  METHOD rtti_get_table_desrc.

    DATA ddtext TYPE c LENGTH 60.
      DATA lan LIKE sy-langu.
          DATA lv_tabname TYPE string.

    IF langu IS NOT SUPPLIED.

      lan = sy-langu.
    ELSE.
      lan = langu.
    ENDIF.

    IF check_abap_cloud( ) IS NOT INITIAL.

      ddtext = tabname.

    ELSE.

      TRY.

          lv_tabname = `dd02t`.
          SELECT SINGLE ddtext
            FROM (lv_tabname) INTO ddtext
            WHERE tabname    = tabname
              AND ddlanguage = lan
            .
        CATCH cx_root ##NO_HANDLER.
          " DD02T not available (e.g. JS transpiler runtime) - fall
          " back to the table name below
      ENDTRY.

    ENDIF.

    IF ddtext IS NOT INITIAL.
      result = ddtext.
    ELSE.
      result = tabname.
    ENDIF.

  ENDMETHOD.

  METHOD bus_search_help_read.

    TYPES temp7 TYPE TABLE OF string.
DATA lt_result_tab TYPE temp7.
    DATA ls_comp       TYPE abap_componentdescr.
    DATA lt_comps      TYPE abap_component_tab.
    DATA lo_datadescr  TYPE REF TO cl_abap_datadescr.
    DATA lr_line       TYPE REF TO data.

    " data ls_shlp type shlp_descr.

    DATA lr_shlp       TYPE REF TO data.

    DATA lv_type TYPE string.
    FIELD-SYMBOLS <shlp> TYPE any.
    DATA lv_tabname TYPE c LENGTH 30.
    DATA lv_fieldname TYPE c LENGTH 30.
      DATA lv_fm TYPE string.
        DATA lr_t_shlp TYPE REF TO data.
        DATA lv_type2 TYPE string.
        FIELD-SYMBOLS <shlp2> TYPE STANDARD TABLE.
        FIELD-SYMBOLS <row2> TYPE any.
      DATA temp75 LIKE LINE OF ms_shlp-interface.
      DATA r_interface LIKE REF TO temp75.
        FIELD-SYMBOLS <any> TYPE any.
        FIELD-SYMBOLS <value> TYPE any.
    DATA interface LIKE LINE OF ms_shlp-interface.
        DATA temp76 TYPE z2ui5_cl_popup_context=>ty_shlp_descr-selopt.
        DATA temp77 LIKE LINE OF temp76.
        DATA temp5 TYPE z2ui5_cl_popup_context=>ty_ddshselopt-option.
    DATA fieldrop LIKE LINE OF ms_shlp-fieldprop.
      DATA default_value LIKE fieldrop-defaultval.
      DATA temp78 TYPE z2ui5_cl_popup_context=>ty_shlp_descr-selopt.
      DATA temp79 LIKE LINE OF temp78.
      DATA temp6 TYPE z2ui5_cl_popup_context=>ty_ddshselopt-option.
    DATA field_props LIKE LINE OF ms_shlp-fieldprop.
      DATA temp80 TYPE z2ui5_cl_popup_context=>ty_s_dfies_2.
      DATA temp81 TYPE z2ui5_cl_popup_context=>ty_s_dfies_2.
      DATA descption LIKE temp80.
    DATA temp82 LIKE sy-subrc.
    DATA temp83 LIKE sy-subrc.
    DATA temp84 LIKE sy-tabix.
    DATA strucdescr TYPE REF TO cl_abap_structdescr.
    DATA tabdescr TYPE REF TO cl_abap_tabledescr.
    FIELD-SYMBOLS <fs_target_tab> TYPE STANDARD TABLE.
    DATA result_line LIKE LINE OF lt_result_tab.
      FIELD-SYMBOLS <fs_line> TYPE data.
      DATA result_desc LIKE LINE OF mt_result_desc.
        FIELD-SYMBOLS <line_content> TYPE any.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE any.
      FIELD-SYMBOLS <row> TYPE any.
    lv_type = `SHLP_DESCR`.
    CREATE DATA lr_shlp TYPE (lv_type).

    ASSIGN lr_shlp->* TO <shlp>.



    lv_tabname = mv_table.
    lv_fieldname = mv_fname.

    IF ms_shlp IS INITIAL.
      " Suchhilfe lesen

      lv_fm = `F4IF_DETERMINE_SEARCHHELP`.
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
          error_message     = 4
          OTHERS            = 5.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE z2ui5_cx_popup_error
          EXPORTING
            val = |F4IF_DETERMINE_SEARCHHELP failed for { lv_tabname }-{ lv_fieldname }|.
      ENDIF.
      MOVE-CORRESPONDING <shlp> TO ms_shlp.

      IF ms_shlp-intdescr-issimple = abap_false.



        lv_type2 = `SHLP_DESCT`.
        CREATE DATA lr_t_shlp TYPE (lv_type2).

        ASSIGN lr_t_shlp->* TO <shlp2>.

        lv_fm = `F4IF_EXPAND_SEARCHHELP`.
        " shlp_top is typed SHLP_DESCR - pass the DDIC-typed result of
        " F4IF_DETERMINE_SEARCHHELP, not the local rebuild ms_shlp: a
        " function module called by name wants the exact type
        CALL FUNCTION lv_fm
          EXPORTING
            shlp_top      = <shlp>
          IMPORTING
            shlp_tab      = <shlp2>
          EXCEPTIONS
            " a MESSAGE TYPE E would end the request - on failure <shlp2>
            " stays empty and the collective help is used as it is
            error_message = 1
            OTHERS        = 2.


        READ TABLE <shlp2> INDEX 1 ASSIGNING <row2>.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING <row2> TO ms_shlp.
        ENDIF.
      ENDIF.
    ENDIF.

    IF mr_data IS BOUND.
      " Values from Caller app to Interface Values


      LOOP AT ms_shlp-interface REFERENCE INTO r_interface WHERE value IS INITIAL.


        ASSIGN mr_data->* TO <any>.

        ASSIGN COMPONENT r_interface->shlpfield OF STRUCTURE <any> TO <value>.

        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        r_interface->value = <value>.

      ENDLOOP.
    ENDIF.

    " Interface Fixed Values to Selopt

    LOOP AT ms_shlp-interface INTO interface.

      " Match the name of the SH Field to the Input field name
      IF interface-valfield = mv_fname.
        mv_shlpfield = interface-shlpfield.
      ENDIF.

      IF interface-value IS NOT INITIAL.


        CLEAR temp76.
        temp76 = ms_shlp-selopt.

        temp77-shlpfield = interface-shlpfield.
        temp77-shlpname = interface-valtabname.

        IF interface-value CA `*`.
          temp5 = `CP`.
        ELSE.
          temp5 = `EQ`.
        ENDIF.
        temp77-option = temp5.
        temp77-sign = `I`.
        temp77-low = interface-value.
        INSERT temp77 INTO TABLE temp76.
        ms_shlp-selopt = temp76.

      ENDIF.

    ENDLOOP.


    LOOP AT ms_shlp-fieldprop INTO fieldrop.

      IF fieldrop-defaultval IS INITIAL.
        CONTINUE.
      ENDIF.


      default_value = fieldrop-defaultval.
      default_value = replace( val = default_value sub = `'` with = `` occ = 0 ).


      CLEAR temp78.
      temp78 = ms_shlp-selopt.

      temp79-shlpfield = fieldrop-fieldname.

      IF fieldrop-defaultval CA `*`.
        temp6 = `CP`.
      ELSE.
        temp6 = `EQ`.
      ENDIF.
      temp79-option = temp6.
      temp79-sign = `I`.
      temp79-low = default_value.
      INSERT temp79 INTO TABLE temp78.
      ms_shlp-selopt = temp78.

    ENDLOOP.

    CREATE DATA lr_shlp TYPE (lv_type).
    ASSIGN lr_shlp->* TO <shlp>.
    CLEAR: <shlp>.
    MOVE-CORRESPONDING ms_shlp TO <shlp>.

    lv_fm = `F4IF_SELECT_VALUES`.
    CALL FUNCTION lv_fm
      EXPORTING
        shlp           = <shlp>
        maxrecords     = iv_max_rows
        sort           = space
        call_shlp_exit = abap_true
      TABLES
        record_tab     = lt_result_tab
        recdescr_tab   = mt_result_desc
      EXCEPTIONS
        " a search help exit may send MESSAGE TYPE E - without this the
        " request ends there
        error_message  = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE z2ui5_cx_popup_error
        EXPORTING
          val = |F4IF_SELECT_VALUES failed for { ms_shlp-shlpname }|.
    ENDIF.

    SORT ms_shlp-fieldprop BY shlplispos ASCENDING.


    LOOP AT ms_shlp-fieldprop INTO field_props WHERE shlplispos IS NOT INITIAL.


      CLEAR temp80.

      READ TABLE mt_result_desc INTO temp81 WITH KEY fieldname = field_props-fieldname.
      IF sy-subrc = 0.
        temp80 = temp81.
      ENDIF.

      descption = temp80.

      ls_comp-name  = descption-fieldname.
      ls_comp-type ?= cl_abap_datadescr=>describe_by_name( descption-rollname ).
      INSERT ls_comp INTO TABLE lt_comps.

    ENDLOOP.




    temp83 = sy-subrc.
    temp84 = sy-tabix.
    READ TABLE lt_comps WITH KEY name = `ROW_ID` TRANSPORTING NO FIELDS.
    temp82 = sy-subrc.
    sy-subrc = temp83.
    sy-tabix = temp84.
    IF NOT temp82 = 0.
      lo_datadescr ?= cl_abap_datadescr=>describe_by_name( `INT4` ).
      ls_comp-name  = `ROW_ID`.
      ls_comp-type ?= lo_datadescr.
      INSERT ls_comp INTO TABLE lt_comps.
    ENDIF.


    strucdescr = cl_abap_structdescr=>create( lt_comps ).


    tabdescr = cl_abap_tabledescr=>create( strucdescr ).

    IF mt_data IS NOT BOUND.
      CREATE DATA mt_data TYPE HANDLE tabdescr.
    ENDIF.


    ASSIGN mt_data->* TO <fs_target_tab>.

    CLEAR <fs_target_tab>.

    " we don`t want to lose all inputs in row ...
    IF ms_data_row IS NOT BOUND.
      CREATE DATA ms_data_row TYPE HANDLE strucdescr.
    ENDIF.


    LOOP AT lt_result_tab INTO result_line.

      CREATE DATA lr_line TYPE HANDLE strucdescr.

      ASSIGN lr_line->* TO <fs_line>.


      LOOP AT mt_result_desc INTO result_desc.


        ASSIGN COMPONENT result_desc-fieldname OF STRUCTURE <fs_line>
               TO <line_content>.

        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF result_desc-leng < result_desc-intlen.
          " internal and external representation differ
          " Unicode: halve the offset
          result_desc-offset = result_desc-offset / 2.
        ENDIF.

        TRY.
            <line_content> = result_line+result_desc-offset(result_desc-outputlen).
          CATCH cx_root.

            TRY.
                " String table will crash if value length <> outputlen
                <line_content> = result_line+result_desc-offset.
              CATCH cx_root ##NO_HANDLER.
            ENDTRY.
        ENDTRY.

      ENDLOOP.

      INSERT <fs_line> INTO TABLE <fs_target_tab>.

    ENDLOOP.

    " Set default values
    LOOP AT ms_shlp-interface INTO interface.

      IF interface-value IS NOT INITIAL.

        UNASSIGN <any>.
        ASSIGN ms_data_row->* TO <any>.
        ASSIGN COMPONENT interface-shlpfield OF STRUCTURE <any> TO <value>.

        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        <value> = interface-value.

      ENDIF.

    ENDLOOP.




    ASSIGN mt_data->* TO <tab>.

    LOOP AT <tab> ASSIGNING <line>.


      UNASSIGN <row>.
      ASSIGN COMPONENT 'ROW_ID' OF STRUCTURE <line> TO <row>.
      IF <row> IS ASSIGNED.
        <row> = sy-tabix.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD tab_get_where_by_dfies.

    DATA val TYPE string.

    DATA temp85 LIKE LINE OF it_dfies.
    DATA dfies LIKE REF TO temp85.
      FIELD-SYMBOLS <row> TYPE data.
      FIELD-SYMBOLS <value> TYPE any.
        DATA and TYPE string.
    LOOP AT it_dfies REFERENCE INTO dfies.

      IF NOT ( dfies->keyflag = abap_true OR dfies->fieldname = mv_check_tab_field ).
        CONTINUE.
      ENDIF.


      ASSIGN ms_data_row->* TO <row>.


      UNASSIGN <value>.
      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <row> TO <value>.
      IF <value> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      IF <value> IS INITIAL.
        CONTINUE.
      ENDIF.

      IF result IS NOT INITIAL.

        and = ` AND `.
      ENDIF.

      val = <value>.

      " the value goes into a literal of a dynamic WHERE: the escape character
      " first, then the LIKE wildcards, so that they match only themselves,
      " and a quote is doubled so that it cannot end the literal
      val = replace( val = val sub = `#` with = `##` occ = 0 ).
      val = replace( val = val sub = `%` with = `#%` occ = 0 ).
      val = replace( val = val sub = `_` with = `#_` occ = 0 ).
      val = replace( val = val sub = `'` with = `''` occ = 0 ).

      result = |{ result }{ and } ( { dfies->fieldname } LIKE '%{ val }%' ESCAPE '#' )|.

    ENDLOOP.

  ENDMETHOD.

  METHOD _get_e071k_tabkey.

    DATA lv_type       TYPE c LENGTH 1.
    DATA lv_tabkey     TYPE c LENGTH 999.
    DATA lv_tabkey_len TYPE i.
    DATA lv_field_len  TYPE i.

    DATA s_dfies LIKE LINE OF dfies.
      FIELD-SYMBOLS <value> TYPE any.
      DATA lo_value_type TYPE REF TO cl_abap_typedescr.
    LOOP AT dfies INTO s_dfies WHERE keyflag = abap_true.


      UNASSIGN <value>.
      ASSIGN COMPONENT s_dfies-fieldname OF STRUCTURE line TO <value>.
      IF <value> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.


      lo_value_type = cl_abap_typedescr=>describe_by_data( <value> ).
      lv_type = lo_value_type->type_kind.

      IF lv_type NA 'CDNT'.
        lv_tabkey+lv_tabkey_len = '*'.
        rv_tabkey = lv_tabkey.
        RETURN.
      ENDIF.

      lv_field_len = lo_value_type->length / cv_char_util_charsize.
      lv_tabkey+lv_tabkey_len(lv_field_len) = <value>.
      lv_tabkey_len = lv_tabkey_len + lv_field_len.

    ENDLOOP.

    " TABKEY is CHAR120: a longer key becomes generic at its last position.
    " Cutting at the first `_` anywhere in the key put the `*` outside the
    " 120 characters when that `_` came late, and the key was not generic
    IF lv_tabkey_len > 119.
      lv_tabkey+119 = '*'.
    ENDIF.
    rv_tabkey = lv_tabkey.
  ENDMETHOD.

  METHOD bus_tr_add.
      FIELD-SYMBOLS <e071> TYPE any.
      FIELD-SYMBOLS <t_e071k> TYPE STANDARD TABLE.
      FIELD-SYMBOLS <t_e071> TYPE STANDARD TABLE.
      DATA r_e071k TYPE REF TO data.
      DATA r_e071 TYPE REF TO data.
      DATA fb1 TYPE c LENGTH 27.
      DATA fb2 TYPE c LENGTH 25.
    IF check_abap_cloud( ) IS NOT INITIAL.

    ELSE.





      " We need to set the MANDT is necessary
      set_mandt( ir_data ).


      r_e071k = _set_e071k( ir_data      = ir_data
                                  iv_tabname   = iv_tabname
                                  is_transport = is_transport ).
      ASSIGN r_e071k->* TO <e071>.
      IF <e071> IS INITIAL.
        RETURN.
      ENDIF.


      r_e071 = _set_e071( iv_tabname   = iv_tabname
                                is_transport = is_transport ).

      ASSIGN r_e071k->* TO <t_e071k>.
      ASSIGN r_e071->* TO <t_e071>.


      fb1 = 'TR_APPEND_TO_COMM_OBJS_KEYS'.
      CALL FUNCTION fb1
        EXPORTING
          " the task: the keys belong to it, E071/E071K carry its number
          wi_trkorr     = is_transport-task
          iv_dialog     = abap_false
        TABLES
          wt_e071       = <t_e071>
          wt_e071k      = <t_e071k>
        EXCEPTIONS
          error_message = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE z2ui5_cx_popup_error
          EXPORTING
            val = |{ fb1 }: { sy-msgid } { sy-msgno } { sy-msgv1 } { sy-msgv2 } { sy-msgv3 } { sy-msgv4 }|.
      ENDIF.


      fb2 = 'TR_SORT_AND_COMPRESS_COMM'.
      CALL FUNCTION fb2
        EXPORTING
          iv_trkorr     = is_transport-task
        EXCEPTIONS
          error_message = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE z2ui5_cx_popup_error
          EXPORTING
            val = |{ fb2 }: { sy-msgid } { sy-msgno } { sy-msgv1 } { sy-msgv2 } { sy-msgv3 } { sy-msgv4 }|.
      ELSEIF iv_commit = abap_true.
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

    DATA t_comp TYPE abap_component_tab.
        DATA struct_desc TYPE REF TO cl_abap_structdescr.
        DATA table_desc TYPE REF TO cl_abap_tabledescr.
        DATA x TYPE REF TO cx_root.
    DATA dfies TYPE z2ui5_cl_popup_context=>ty_t_dfies.
    t_comp = rtti_get_t_attri_by_table_name( 'E071K' ).

    TRY.


        struct_desc = cl_abap_structdescr=>create( t_comp ).


        table_desc = cl_abap_tabledescr=>create( p_line_type  = struct_desc
                                                       p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA t_e071k TYPE HANDLE table_desc.
        CREATE DATA s_e071k TYPE HANDLE struct_desc.

        ASSIGN t_e071k->* TO <t_e071k>.
        ASSIGN s_e071k->* TO <s_e071k>.


      CATCH cx_root INTO x.
        " without the E071K line type nothing below can be filled - say why
        RAISE EXCEPTION TYPE z2ui5_cx_popup_error
          EXPORTING
            val      = |E071K line type could not be built: { x->get_text( ) }|
            previous = x.
    ENDTRY.


    dfies = rtti_get_t_dfies_by_table_name( iv_tabname ).

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

      UNASSIGN <value>.
      ASSIGN COMPONENT 'TABKEY' OF STRUCTURE <s_e071k> TO <value>.
      IF <value> IS NOT ASSIGNED.
        RETURN.
      ELSE.
        <value> = _get_e071k_tabkey( dfies = dfies
                                     line  = <line> ).
      ENDIF.

      INSERT <s_e071k> INTO TABLE <t_e071k>.

    ENDLOOP.

    result = t_e071k.
  ENDMETHOD.

  METHOD _set_e071.

    DATA t_e071 TYPE REF TO data.
    DATA s_e071 TYPE REF TO data.

    FIELD-SYMBOLS <t_e071> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <s_e071> TYPE any.
    FIELD-SYMBOLS <value>  TYPE any.

    DATA t_comp TYPE abap_component_tab.
        DATA struct_desc_new TYPE REF TO cl_abap_structdescr.
        DATA table_desc_new TYPE REF TO cl_abap_tabledescr.
    t_comp = rtti_get_t_attri_by_table_name( 'E071' ).

    TRY.


        struct_desc_new = cl_abap_structdescr=>create( t_comp ).


        table_desc_new = cl_abap_tabledescr=>create( p_line_type  = struct_desc_new
                                                           p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA t_e071 TYPE HANDLE table_desc_new.
        CREATE DATA s_e071 TYPE HANDLE struct_desc_new.

        ASSIGN t_e071->* TO <t_e071>.
        ASSIGN s_e071->* TO <s_e071>.

      CATCH cx_root ##NO_HANDLER.
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

    INSERT <s_e071> INTO TABLE <t_e071>.

    result = t_e071.

  ENDMETHOD.

  METHOD _read_e070.

    DATA lo_tab  TYPE REF TO data.
    DATA lo_line TYPE REF TO data.
    DATA ls_data TYPE ty_s_transport.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line>  TYPE any.
    FIELD-SYMBOLS <value> TYPE any.

    DATA table_name TYPE c LENGTH 4.
        DATA t_comp TYPE abap_component_tab.
        DATA new_struct_desc TYPE REF TO cl_abap_structdescr.
        DATA new_table_desc TYPE REF TO cl_abap_tabledescr.
        DATA where TYPE string.
    table_name = 'E070'.

    TRY.

        t_comp = rtti_get_t_attri_by_table_name( table_name ).


        new_struct_desc = cl_abap_structdescr=>create( t_comp ).


        new_table_desc = cl_abap_tabledescr=>create( p_line_type  = new_struct_desc
                                                           p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA lo_tab TYPE HANDLE new_table_desc.
        CREATE DATA lo_line TYPE HANDLE new_struct_desc.

        ASSIGN lo_tab->* TO <table>.
        ASSIGN lo_line->* TO <line>.


        where =
        |( TRFUNCTION EQ 'Q' ) AND ( TRSTATUS EQ 'D' ) AND ( KORRDEV EQ 'CUST' ) AND ( AS4USER EQ '{ sy-uname }' )|.

        SELECT trkorr
               trfunction
               trstatus
               tarsystem
               korrdev
               as4user
               as4date
               as4time
               strkorr
          FROM (table_name) INTO TABLE <table>
          WHERE (where)
          .
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
    IF <table> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    " the rows are TASKS (TRFUNCTION Q): TRKORR is the task, STRKORR the
    " request it belongs to - the other way round, as it was, bus_tr_add
    " wrote the keys under the request number
    LOOP AT <table> INTO <line>.

      UNASSIGN <value>.
      ASSIGN COMPONENT 'TRKORR' OF STRUCTURE <line> TO <value>.
      IF <value> IS NOT ASSIGNED.
        CONTINUE.
      ELSE.
        ls_data-task = <value>.
      ENDIF.

      UNASSIGN <value>.

      ASSIGN COMPONENT 'STRKORR' OF STRUCTURE <line> TO <value>.
      IF <value> IS NOT ASSIGNED.
        CONTINUE.
      ELSE.
        ls_data-transport = <value>.
      ENDIF.

      UNASSIGN <value>.

      INSERT ls_data INTO TABLE mt_data.

    ENDLOOP.

  ENDMETHOD.

  METHOD bus_tr_read.
      DATA lo_tab TYPE REF TO data.
      DATA lo_line TYPE REF TO data.
      FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
      FIELD-SYMBOLS <line> TYPE any.
      FIELD-SYMBOLS <value> TYPE any.
      DATA table_name TYPE c LENGTH 4.
          DATA t_comp TYPE abap_component_tab.
          DATA new_struct_desc TYPE REF TO cl_abap_structdescr.
          DATA new_table_desc TYPE REF TO cl_abap_tabledescr.
          DATA index TYPE i.
          DATA line LIKE LINE OF mt_data.
              DATA where TYPE string.
        DATA temp86 TYPE sxco_transport.
        DATA lv_trkorr LIKE temp86.
        DATA lv_logon_langu TYPE abap_bool.
        DATA temp5 TYPE xsdboolean.
        FIELD-SYMBOLS <ls_data> LIKE LINE OF mt_data.

    IF check_abap_cloud( ) IS NOT INITIAL.

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








      _read_e070( CHANGING mt_data = mt_data ).
      " no task - an empty dynamic WHERE would read every text of E07T
      IF mt_data IS INITIAL.
        RETURN.
      ENDIF.


      table_name = 'E07T'.

      TRY.

          t_comp = rtti_get_t_attri_by_table_name( table_name ).


          new_struct_desc = cl_abap_structdescr=>create( t_comp ).


          new_table_desc = cl_abap_tabledescr=>create( p_line_type  = new_struct_desc
                                                             p_table_kind = cl_abap_tabledescr=>tablekind_std ).

          CREATE DATA lo_tab TYPE HANDLE new_table_desc.
          CREATE DATA lo_line TYPE HANDLE new_struct_desc.

          ASSIGN lo_tab->* TO <table>.
          ASSIGN lo_line->* TO <line>.


          index = 0.

          " the description shown is the one of the request

          LOOP AT mt_data INTO line.
            index = index + 1.
            IF index = 1.

              where = |TRKORR EQ { filter_sql_quote( line-transport ) }|.
            ELSE.
              where = |{ where } OR TRKORR EQ { filter_sql_quote( line-transport ) }|.
            ENDIF.
            where = |( { where } )|.
          ENDLOOP.

          SELECT trkorr
                 langu
                 as4text
            FROM (table_name) INTO TABLE <table>
            WHERE (where)
            .
          IF sy-subrc <> 0.
            RETURN.
          ENDIF.

        CATCH cx_root ##NO_HANDLER.
      ENDTRY.

      IF <table> IS NOT ASSIGNED.
        RETURN.
      ENDIF.

      " every task of a request gets its text - the logon language wins,
      " another language only fills a text that is still empty
      LOOP AT <table> INTO <line>.

        UNASSIGN <value>.
        ASSIGN COMPONENT 'TRKORR' OF STRUCTURE <line> TO <value>.
        IF <value> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.

        temp86 = <value>.

        lv_trkorr = temp86.

        UNASSIGN <value>.
        ASSIGN COMPONENT 'LANGU' OF STRUCTURE <line> TO <value>.


        temp5 = boolc( <value> IS ASSIGNED AND <value> = sy-langu ).
        lv_logon_langu = temp5.

        UNASSIGN <value>.
        ASSIGN COMPONENT 'AS4TEXT' OF STRUCTURE <line> TO <value>.
        IF <value> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.


        LOOP AT mt_data ASSIGNING <ls_data> WHERE transport = lv_trkorr.
          IF <ls_data>-short_description IS INITIAL OR lv_logon_langu = abap_true.
            <ls_data>-short_description = <value>.
          ENDIF.
        ENDLOOP.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD set_mandt.

    FIELD-SYMBOLS <tab>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE any.
      FIELD-SYMBOLS <row> TYPE any.

    ASSIGN ir_data->* TO <tab>.

    LOOP AT <tab> ASSIGNING <line>.


      UNASSIGN <row>.
      ASSIGN COMPONENT `MANDT` OF STRUCTURE <line> TO <row>.
      IF <row> IS ASSIGNED.

        TRY.
            <row> = sy-mandt.
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD file_get_mimetype.

    DATA lv_name TYPE string.
    TYPES temp9 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_parts TYPE temp9.
    DATA lv_ext LIKE LINE OF lt_parts.
    FIELD-SYMBOLS <temp7> LIKE LINE OF lt_parts.
    DATA temp8 LIKE sy-tabix.
    lv_name = to_lower( c_trim( val ) ).


    SPLIT lv_name AT `.` INTO TABLE lt_parts.
    " no name, no extension - an empty name splits into no part at all
    IF lt_parts IS INITIAL.
      result = `application/octet-stream`.
      RETURN.
    ENDIF.



    temp8 = sy-tabix.
    READ TABLE lt_parts INDEX lines( lt_parts ) ASSIGNING <temp7>.
    sy-tabix = temp8.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lv_ext = <temp7>.

    CASE lv_ext.
      WHEN `txt` OR `text` OR `log` OR `md`.
        result = `text/plain`.
      WHEN `html` OR `htm`.
        result = `text/html`.
      WHEN `css`.
        result = `text/css`.
      WHEN `csv`.
        result = `text/csv`.
      WHEN `js` OR `mjs`.
        result = `text/javascript`.
      WHEN `json`.
        result = `application/json`.
      WHEN `xml`.
        result = `application/xml`.
      WHEN `pdf`.
        result = `application/pdf`.
      WHEN `zip`.
        result = `application/zip`.
      WHEN `png`.
        result = `image/png`.
      WHEN `jpg` OR `jpeg`.
        result = `image/jpeg`.
      WHEN `gif`.
        result = `image/gif`.
      WHEN `svg`.
        result = `image/svg+xml`.
      WHEN `webp`.
        result = `image/webp`.
      WHEN `ico`.
        result = `image/x-icon`.
      WHEN `mp3`.
        result = `audio/mpeg`.
      WHEN `mp4`.
        result = `video/mp4`.
      WHEN `woff`.
        result = `font/woff`.
      WHEN `woff2`.
        result = `font/woff2`.
      WHEN `ttf`.
        result = `font/ttf`.
      WHEN `otf`.
        result = `font/otf`.
      WHEN `xlsx`.
        result = `application/vnd.openxmlformats-officedocument.spreadsheetml.sheet`.
      WHEN `xls`.
        result = `application/vnd.ms-excel`.
      WHEN `docx`.
        result = `application/vnd.openxmlformats-officedocument.wordprocessingml.document`.
      WHEN `doc`.
        result = `application/msword`.
      WHEN `pptx`.
        result = `application/vnd.openxmlformats-officedocument.presentationml.presentation`.
      WHEN `ppt`.
        result = `application/vnd.ms-powerpoint`.
      WHEN OTHERS.
        result = `application/octet-stream`.
    ENDCASE.

  ENDMETHOD.

  METHOD check_msg_container.

    DATA lv_kind TYPE string.
    DATA temp6 TYPE xsdboolean.
    lv_kind = rtti_get_type_kind( val ).

    temp6 = boolc( lv_kind = cl_abap_datadescr=>typekind_table OR lv_kind = cl_abap_datadescr=>typekind_struct1 OR lv_kind = cl_abap_datadescr=>typekind_struct2 OR lv_kind = cl_abap_datadescr=>typekind_oref ).
    result = temp6.

  ENDMETHOD.

  METHOD filter_token_fill.

    " built by concatenation, not REPLACE ... WITH: LOW and HIGH are data,
    " and the transpiled runtime reads a WITH text like a regex replacement
    " ( `$&`, `$$`, `\{` change - backlog item runtime-replace-with-literal ).
    " HIGH first: a LOW that spells `{HIGH}` is then never substituted a
    " second time ( {LOW} stands before {HIGH} in every template )
    DATA lv_off  TYPE i.
    DATA lv_rest TYPE i.

    result = template.
    lv_off = find( val = result
                   sub = `{HIGH}` ).
    IF lv_off >= 0.
      lv_rest = lv_off + 6.
      result = substring( val = result
                          len = lv_off ) && high && substring( val = result
                                                               off = lv_rest ).
    ENDIF.
    lv_off = find( val = result
                   sub = `{LOW}` ).
    IF lv_off >= 0.
      lv_rest = lv_off + 5.
      result = substring( val = result
                          len = lv_off ) && low && substring( val = result
                                                              off = lv_rest ).
    ENDIF.

  ENDMETHOD.

  METHOD itab_filter_check_field.

    " a table, a deep structure or a reference cannot go into a string
    " template - with no field list every component is visited, and a
    " master-detail row (a table of children inside the row) dumped the
    " whole filter. Such a component holds no text to match
    IF rtti_check_printable( field ) = abap_false.
      RETURN.
    ENDIF.

    result = itab_filter_check_text( text        = |{ field }|
                                     search      = search
                                     ignore_case = ignore_case ).

  ENDMETHOD.

  METHOD itab_filter_check_text.

    IF ignore_case = abap_true.
      IF to_upper( text ) CS search.
        result = abap_true.
      ENDIF.
    ELSEIF find( val = text
                 sub = search ) >= 0.
      result = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD msg_get_by_oref_attri.

    FIELD-SYMBOLS <comp> TYPE any.
    DATA lt_attri_o TYPE abap_attrdescr_tab.
    DATA temp87 LIKE LINE OF lt_attri_o.
    DATA ls_attri_o LIKE REF TO temp87.

    result = is_msg.

    lt_attri_o = rtti_get_t_attri_by_oref( io_obj ).


    LOOP AT lt_attri_o REFERENCE INTO ls_attri_o "#EC CI_SORTSEQ
         WHERE visibility = cv_objectdescr_public.
      " UNASSIGN + IS ASSIGNED, not sy-subrc - the loop-carried case (see
      " error_get_attributes). Here a stale 0 maps the PREVIOUS attribute's
      " value onto this one's name, i.e. a message id, number or placeholder
      " taken from the wrong attribute
      UNASSIGN <comp>.
      ASSIGN io_obj->(ls_attri_o->name) TO <comp>.
      IF <comp> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      result = msg_map( name = ls_attri_o->name
                        val  = <comp>
                        msg  = result ).
    ENDLOOP.

  ENDMETHOD.

  METHOD msg_get_internal_tab.

    " the RAP shape is a property of the ROW TYPE: decided on the first
    " structured row and handed down, not asked of every row - the check
    " describes every table component of a row, which a business table of a
    " few thousand rows paid a few thousand times before the box even
    " decided it carries no message at all
    DATA lv_rap LIKE abap_false.
    DATA lv_first LIKE abap_true.
    FIELD-SYMBOLS <row> TYPE ANY.
        DATA lv_row_kind TYPE string.
    lv_rap = abap_false.

    lv_first = abap_true.

    LOOP AT it_tab ASSIGNING <row>.
      IF lv_first = abap_true.
        lv_first = abap_false.

        lv_row_kind = rtti_get_type_kind( <row> ).
        IF lv_row_kind = cl_abap_datadescr=>typekind_struct1
            OR lv_row_kind = cl_abap_datadescr=>typekind_struct2.
          lv_rap = check_is_rap_struct( <row> ).
        ENDIF.
      ENDIF.
      IF lv_rap = abap_true AND <row> IS NOT INITIAL.
        INSERT LINES OF msg_get_rap( <row> ) INTO TABLE result.
      ELSE.
        INSERT LINES OF msg_get_internal( val          = <row>
                                          iv_check_rap = abap_false ) INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD filter_sql_quote.

    DATA temp88 TYPE string.
    temp88 = val.
    result = |'{ replace( val  = temp88
                          sub  = `'`
                          with = `''`
                          occ  = 0 ) }'|.

  ENDMETHOD.

  METHOD error_get_attributes.

    " declared, NOT `DATA(lv_name) = CONV string( lr_attri->name )`: a CONV
    " that is the whole right-hand side of an assignment is "Redundant
    " conversion for type STRING" in SLIN - the assignment converts by
    " itself. The variable stays a string on purpose: the RTTI name is a
    " CHAR field whose trailing blanks have no business in the rendered name
    DATA lv_name TYPE string.

    FIELD-SYMBOLS <comp> TYPE any.
        DATA lt_attri TYPE abap_attrdescr_tab.
    DATA temp89 LIKE LINE OF lt_attri.
    DATA lr_attri LIKE REF TO temp89.
      DATA temp90 TYPE z2ui5_cl_popup_context=>ty_s_name_value.

    IF val IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.

        lt_attri = rtti_get_t_attri_by_oref( val ).
      CATCH cx_root.
        RETURN.
    ENDTRY.



    LOOP AT lt_attri REFERENCE INTO lr_attri "#EC CI_SORTSEQ
         WHERE visibility  = cv_objectdescr_public
           AND is_constant = abap_false
           AND is_class    = abap_false.

      CASE lr_attri->name.
          " rendered by the caller (PREVIOUS as the next chain entry,
          " KERNEL_ERRID as its own line) or without information value
        WHEN `PREVIOUS` OR `TEXTID` OR `IS_RESUMABLE` OR `KERNEL_ERRID`.
          CONTINUE.
      ENDCASE.

      lv_name = lr_attri->name.
      " UNASSIGN + IS ASSIGNED, not sy-subrc - see unassign_data for the
      " release behaviour (#1937). INSIDE A LOOP the stale-subrc reading is
      " not just "skipped one attribute": a failed ASSIGN leaves the symbol
      " on the PREVIOUS iteration's attribute, so a stale 0 renders that
      " value under this attribute's name - a wrong value in the error dump,
      " which is the one place a reader has nothing else to go by
      UNASSIGN <comp>.
      ASSIGN val->(lv_name) TO <comp>.
      IF <comp> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      IF rtti_check_printable( <comp> ) = abap_false OR <comp> IS INITIAL.
        CONTINUE.
      ENDIF.


      CLEAR temp90.
      temp90-n = lv_name.
      temp90-v = c_trim( |{ <comp> }| ).
      INSERT temp90 INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD error_get_source_position.

    " typed like the EXPORTING parameters of get_source_position (syrepid is
    " CHAR40) - they are passed by reference, so a string would not be
    " type-compatible here
    DATA lv_program TYPE c LENGTH 40.
    DATA lv_include TYPE c LENGTH 40.
    DATA lv_line    TYPE i.

    IF val IS NOT BOUND.
      RETURN.
    ENDIF.

    " open-abap - the transpiled JS runtime behind the node tests - implements
    " get_source_position by reading a field that only the RAISE statement
    " writes, and fails with a JS TypeError for every other exception (a
    " runtime-thrown one, or one built with NEW). That error is not an ABAP
    " exception, so the TRY below cannot intercept it and the caller would go
    " down instead of reporting the error it was diagnosing. sy-saprl is
    " `OPEN` on that runtime and a real release everywhere else.
    IF sy_get( )-saprl = `OPEN`.
      RETURN.
    ENDIF.

    TRY.
        val->get_source_position( IMPORTING program_name = lv_program
                                            include_name = lv_include
                                            source_line  = lv_line ).
      CATCH cx_root ##NO_HANDLER.
        " never let the diagnostic be the reason a caller fails
    ENDTRY.

    IF lv_program IS INITIAL AND lv_line IS INITIAL.
      RETURN.
    ENDIF.

    result = c_trim( lv_program ).
    " the include is the interesting part for a class (...CM001 = the method
    " that raised); repeating it when it equals the program adds nothing
    IF lv_include IS NOT INITIAL AND lv_include <> lv_program.
      result = |{ result } / { c_trim( lv_include ) }|.
    ENDIF.
    IF lv_line IS NOT INITIAL.
      result = |{ result } / line { lv_line }|.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
