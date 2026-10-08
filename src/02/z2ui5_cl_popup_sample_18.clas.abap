CLASS z2ui5_cl_popup_sample_18 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS nav_to_output
      IMPORTING
        as_page TYPE abap_bool.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_sample_18 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.
      DATA view TYPE REF TO z2ui5_cl_ui5_view_builder.

    me->client = client.

    IF client->check_on_navigated( ) IS NOT INITIAL.


      view = z2ui5_cl_ui5_view_builder=>factory(
                       )->ele( n = `View` ns = `mvc`
                       )->a( n = `xmlns` v = `sap.m`
                       )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`
                       )->a( n = `displayBlock` v = `true`
                       )->a( n = `height` v = `100%` ).
      view->ele( `Shell`
          )->ele( `Page`
          )->a( n = `title` v = `abap2UI5 - CL_DEMO_OUTPUT`
          )->a( n = `navButtonPress` v = client->_event_nav_app_leave( )
          )->a( n = `showNavButton` b = client->check_app_prev_stack( )
          )->tag( `Button`
          )->a( n = `text` v = `Open in Popup`
          )->a( n = `press` v = client->_event( `POPUP` )
          )->tag( `Button`
          )->a( n = `text` v = `Open as Fullscreen App`
          )->a( n = `press` v = client->_event( `FULLSCREEN` ) ).
      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `POPUP` ) IS NOT INITIAL.
      nav_to_output( abap_false ).

    ELSEIF client->check_on_event( `FULLSCREEN` ) IS NOT INITIAL.
      nav_to_output( abap_true ).

    ENDIF.

  ENDMETHOD.


  METHOD nav_to_output.

    TYPES: BEGIN OF ty_s_carrier,
             carrid TYPE c LENGTH 3,
             name   TYPE string,
             url    TYPE string,
           END OF ty_s_carrier.
    DATA t_carriers TYPE STANDARD TABLE OF ty_s_carrier WITH DEFAULT KEY.
    DATA temp1 LIKE t_carriers.
    DATA temp2 LIKE LINE OF temp1.
    DATA xml TYPE string.
    DATA output TYPE REF TO object.
    DATA classname TYPE string.
    CLEAR temp1.

    temp2-carrid = `AA`.
    temp2-name = `American Airlines`.
    temp2-url = `http://www.aa.com`.
    INSERT temp2 INTO TABLE temp1.
    temp2-carrid = `LH`.
    temp2-name = `Lufthansa`.
    temp2-url = `http://www.lufthansa.com`.
    INSERT temp2 INTO TABLE temp1.
    temp2-carrid = `SQ`.
    temp2-name = `Singapore Airlines`.
    temp2-url = `http://www.singaporeair.com`.
    INSERT temp2 INTO TABLE temp1.
    t_carriers = temp1.


    xml = `<?xml version="1.0" encoding="UTF-8"?>` &&
                `<flightplan>` &&
                `<flight carrid="LH" connid="0400" cityfrom="FRANKFURT" cityto="NEW YORK"/>` &&
                `<flight carrid="AA" connid="0017" cityfrom="NEW YORK" cityto="SAN FRANCISCO"/>` &&
                `</flightplan>`.

    " CL_DEMO_OUTPUT is a classic ABAP class (not released for ABAP Cloud),
    " so it is instantiated dynamically here to keep the sample portable.
    " The popup itself accepts the output generically (TYPE REF TO object).



    classname = `CL_DEMO_OUTPUT`.
    CALL METHOD (classname)=>(`NEW`)
      RECEIVING
        output = output.
    CALL METHOD output->(`WRITE_TEXT`)
      EXPORTING
        text = `The HTML below is produced by the standard SAP class CL_DEMO_OUTPUT` &&
               ` and rendered inside abap2UI5 - either as a popup or as a fullscreen` &&
               ` app with a back button. It contains text, table data and XML.`.
    CALL METHOD output->(`WRITE_DATA`)
      EXPORTING
        value = t_carriers.
    CALL METHOD output->(`WRITE_XML`)
      EXPORTING
        xml = xml.

    client->nav_app_call( z2ui5_cl_popup_demo_output=>factory(
        i_output  = output
        i_as_page = as_page ) ).

  ENDMETHOD.

ENDCLASS.
