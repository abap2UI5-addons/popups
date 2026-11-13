CLASS z2ui5_cl_popup_file_ul DEFINITION PUBLIC.

  PUBLIC SECTION.
    " abap2ui5lint-disable unbound-public-attribute -- a popup's attributes are its interface to the app that called it, which reads them once the popup returns
    INTERFACES z2ui5_if_app.

    CLASS-METHODS factory
      IMPORTING
        i_text                TYPE string DEFAULT `Choose the file to upload:`
        i_title               TYPE string DEFAULT `File Upload`
        i_button_text_confirm TYPE string DEFAULT `OK`
        i_button_text_cancel  TYPE string DEFAULT `Cancel`
        i_path                TYPE string OPTIONAL
        i_multiple            TYPE abap_bool DEFAULT abap_false
        i_file_type           TYPE string OPTIONAL
      RETURNING
        VALUE(r_result)       TYPE REF TO z2ui5_cl_popup_file_ul.

    TYPES:
      "! a picked file - its name without the folder and its bytes
      BEGIN OF ty_s_file,
        name    TYPE string,
        content TYPE xstring,
      END OF ty_s_file.
    TYPES ty_t_file TYPE STANDARD TABLE OF ty_s_file WITH DEFAULT KEY.

    TYPES:
      "! value is the last file as UTF-8 text (empty when it is no text),
      "! name and content its name and bytes - binary files too; files every
      "! file picked (with i_multiple
      "! all of them, else the last one)
      BEGIN OF ty_s_result,
        value           TYPE string,
        check_confirmed TYPE abap_bool,
        name            TYPE string,
        content         TYPE xstring,
        files           TYPE ty_t_file,
      END OF ty_s_result.

    TYPES:
      BEGIN OF ty_s_name,
        name TYPE string,
      END OF ty_s_name.
    TYPES ty_t_name TYPE STANDARD TABLE OF ty_s_name WITH DEFAULT KEY.

    DATA ms_result             TYPE ty_s_result.
    DATA mv_path               TYPE string.
    DATA mv_value              TYPE string.
    DATA check_confirm_enabled TYPE abap_bool.
    "! the names of the picked files, shown under the uploader
    DATA mt_name               TYPE ty_t_name.

    METHODS result
      RETURNING
        VALUE(result) TYPE ty_s_result.
    " abap2ui5lint-enable unbound-public-attribute

  PROTECTED SECTION.
    DATA client              TYPE REF TO z2ui5_if_client.
    DATA title               TYPE string.
    DATA question_text       TYPE string.
    DATA button_text_confirm TYPE string.
    DATA button_text_cancel  TYPE string.
    DATA multiple            TYPE abap_bool.
    DATA file_type           TYPE string.

    METHODS view_display.

    METHODS on_upload.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_file_ul IMPLEMENTATION.

  METHOD factory.

    CREATE OBJECT r_result.
    r_result->title               = i_title.

    r_result->question_text       = i_text.
    r_result->button_text_confirm = i_button_text_confirm.
    r_result->button_text_cancel  = i_button_text_cancel.
    r_result->mv_path             = i_path.
    r_result->multiple            = i_multiple.
    r_result->file_type           = i_file_type.

  ENDMETHOD.

  METHOD result.

    result = ms_result.

  ENDMETHOD.

  METHOD view_display.

    DATA popup TYPE REF TO z2ui5_cl_ui5_view_builder.
    popup = z2ui5_cl_ui5_view_builder=>factory(
                      )->ele( n = `FragmentDefinition` ns = `core`
                      )->a( n = `xmlns` v = `sap.m`
                      )->a( n = `xmlns:core` v = `sap.ui.core`
                      )->a( n = `xmlns:z2ui5` v = `z2ui5.cc`
                      )->ele( `Dialog`
                      )->a( n = `title` t = title
                      )->a( n = `afterClose` v = client->_event( `BUTTON_CANCEL` )
                      )->ele( `content`
                      )->ele( `VBox`
                      )->a( n = `class` v = `sapUiMediumMargin`
                      )->tag( `Label`
                      )->a( n = `text` t = question_text
                      )->tag( n = `FileUploader` ns = `z2ui5`
                      )->a( n = `value` v = client->_bind( mv_value )
                      )->a( n = `path` v = client->_bind( mv_path )
                      )->a( n = `placeholder` v = `filepath here...`
                      )->a( n = `multiple` b = multiple
                      )->a( n = `fileType` t = file_type
                      )->a( n = `upload` v = client->_event( `UPLOAD` )
                      )->ele( `List`
                      )->a( n = `items` v = client->_bind( mt_name )
                      )->a( n = `visible` b = multiple
                      )->a( n = `showNoData` v = `false`
                      )->ele( `items`
                      )->tag( `StandardListItem`
                      )->a( n = `title` v = `{NAME}`
                      )->a( n = `icon` v = `sap-icon://document`
                      )->end(
                      )->end(
                      )->end(
                      )->end(
                      )->ele( `buttons`
                      )->tag( `Button`
                      )->a( n = `text` t = button_text_cancel
                      )->a( n = `press` v = client->_event( `BUTTON_CANCEL` )
                      )->tag( `Button`
                      )->a( n = `text` t = button_text_confirm
                      )->a( n = `press` v = client->_event( `BUTTON_CONFIRM` )
                      )->a( n = `enabled` v = client->_bind( check_confirm_enabled )
                      )->a( n = `type` v = `Emphasized` ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD on_upload.

    DATA ls_file TYPE ty_s_file.
    DATA lv_slash TYPE i.
    DATA temp5 TYPE z2ui5_cl_popup_file_ul=>ty_s_name.

    ls_file-content = z2ui5_cl_popup_context=>conv_get_xstring_by_data_uri( mv_value ).
    " the name without the folder - the browser hands C:\fakepath\name over
    ls_file-name = mv_path.
    REPLACE ALL OCCURRENCES OF `\` IN ls_file-name WITH `/`.

    lv_slash = find( val = ls_file-name sub = `/` occ = -1 ).
    IF lv_slash >= 0.
      ls_file-name = substring( val = ls_file-name off = lv_slash + 1 ).
    ENDIF.

    " the text of a UTF-8 file - a binary one has its bytes only
    TRY.
        ms_result-value = z2ui5_cl_popup_context=>conv_get_string_by_xstring( ls_file-content ).
      CATCH cx_root.
        CLEAR ms_result-value.
    ENDTRY.
    ms_result-name = ls_file-name.
    ms_result-content = ls_file-content.
    IF multiple = abap_false.
      CLEAR: ms_result-files, mt_name.
    ENDIF.
    DELETE ms_result-files WHERE name = ls_file-name.
    DELETE mt_name WHERE name = ls_file-name.
    INSERT ls_file INTO TABLE ms_result-files.

    CLEAR temp5.
    temp5-name = ls_file-name.
    INSERT temp5 INTO TABLE mt_name.
    check_confirm_enabled = abap_true.

    CLEAR mv_value.
    CLEAR mv_path.

  ENDMETHOD.

  METHOD z2ui5_if_app~main.
    DATA lv_event TYPE z2ui5_if_client=>ty_s_get-event.
        DATA temp1 TYPE xsdboolean.

    me->client = client.

    IF client->check_on_init( ) IS NOT INITIAL.
      view_display( ).
      RETURN.
    ENDIF.


    lv_event = client->get( )-event.
    CASE lv_event.

      WHEN `UPLOAD`.
        on_upload( ).

      WHEN `BUTTON_CONFIRM` OR `BUTTON_CANCEL`.

        temp1 = boolc( lv_event = `BUTTON_CONFIRM` ).
        ms_result-check_confirmed = temp1.
        client->popup_destroy( ).
        client->nav_app_leave( ).
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
