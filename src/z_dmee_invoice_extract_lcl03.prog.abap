*&---------------------------------------------------------------------*
*&  Include           Z_DMEE_INVOICE_EXTRACT_LCL03
*&---------------------------------------------------------------------*
CLASS lcl_report DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS get_instance RETURNING VALUE(r_o_report) TYPE REF TO lcl_report.
    "Main processing
    METHODS process.

  PRIVATE SECTION.
    CLASS-DATA: m_o_report TYPE REF TO lcl_report.

    DATA: mt_invoice_header TYPE lcl_tools=>tt_invoice_header.
    DATA: mt_invoice_lines TYPE lcl_tools=>tt_invoice_lines.
    DATA: mt_invoice_deliveries TYPE lcl_tools=>tt_invoice_deliveries.

    DATA: mv_dmee_first_started TYPE xfeld.

    CLASS-DATA m_o_screen TYPE REF TO lcl_screen.
    DATA: mt_dmee_output TYPE tab_dmee_output_file.

    METHODS: constructor.
    METHODS: select_data.

    "selection methods
    METHODS:
      select_invoices,
      select_invoice_lines,
      select_deliveries,
      get_tax_code_for_item.

    "DME handler
    METHODS:
      generate_dmee,
      start_dmee_engine IMPORTING i_s_invoice TYPE zdmee_invoice_header,
      add_dmee_item IMPORTING i_v_invoice_header   TYPE zdmee_invoice_header
                              i_v_invoice_line     TYPE zdmee_invoice_line
                              i_v_invoice_delivery TYPE zdmee_delivery_line OPTIONAL,
      close_engine,
      check_if_new_file_needed IMPORTING i_s_dmee_data              TYPE zdmee_invoice_data
                               RETURNING VALUE(r_v_new_file_needed) TYPE xfeld,
      download_file,
      download_xml_file,
      download_flat_file,
      xml_file_start_download IMPORTING i_v_display_only TYPE xfeld
                                        i_v_filename     TYPE string.
    "ALV display
    METHODS: display_alv.

ENDCLASS.

CLASS lcl_report IMPLEMENTATION.

  METHOD get_instance.

    IF m_o_report IS NOT BOUND.
      CREATE OBJECT m_o_report.
    ENDIF.
    r_o_report = m_o_report.

  ENDMETHOD.

  METHOD constructor.

    me->m_o_screen = lcl_screen=>get_instance( ).

  ENDMETHOD.

  METHOD process.

    select_data( ).   "Data selection
    generate_dmee( ). "Handling of all DMEE functions
    display_alv( ).   "Call LCL_ALV_DISPLAY class

  ENDMETHOD.

  METHOD select_data.

    select_invoices( ).     "Select invoice headers- VBRK
    select_invoice_lines( )."Select invoice lines - VBRP
    select_deliveries( ).   "Select delivery data for invoices - LIKP and LIPS

  ENDMETHOD.

  METHOD select_invoices.

    SELECT vkorg vtweg spart bukrs vbeln fkart fktyp waerk
           fkdat inco1 kurrf zterm ernam erzet kunrg kunag
           stceg fkart_rl bstnk_vf landtx land1 xblnr fksto
           kidno zuonr vbtyp knumv
    FROM vbrk
    INTO TABLE mt_invoice_header
    WHERE vbeln IN m_o_screen->mt_billing_docs
      AND bukrs EQ m_o_screen->mv_company_code
      AND fkdat IN m_o_screen->mt_billing_date
      AND fkart IN m_o_screen->mt_billing_types
      AND vkorg IN m_o_screen->mt_sales_org.
    IF sy-subrc NE 0.

      MESSAGE text-003 TYPE 'I'.
      LEAVE TO SCREEN 0.

    ENDIF.

  ENDMETHOD.

  METHOD select_invoice_lines.

    IF mt_invoice_header IS NOT INITIAL.
      SELECT vbeln, posnr, vrkme, meins, fkimg, ntgew, brgew,
             gewei, volum, voleh, prsdt, netwr, vgbel, wavwr,
             mwsbp, cmpre, vgpos, vgtyp, aubel, aupos, matnr,
             arktx, matkl, werks, pstyv, aland, sktof, skfbp,
             ernam, ean11, fbuda, paobjnr
      FROM vbrp
      INTO TABLE @DATA(l_t_invoice_lines)
            FOR ALL ENTRIES IN @mt_invoice_header
            WHERE vbeln = @mt_invoice_header-vbeln.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING l_t_invoice_lines TO mt_invoice_lines.
      ENDIF.

    ENDIF.

    get_tax_code_for_item( ). "Determine MWZKS from condition lines - KONV

  ENDMETHOD.

  METHOD select_deliveries.
    TYPES: tr_delivery TYPE RANGE OF vbeln_vl.

    "get all deliveries as base for selection
    DATA(l_t_delivery_reference) = VALUE tr_delivery( FOR ls_delivery IN mt_invoice_lines
          WHERE ( vgtyp = 'J' )
            LET sign = 'I'
                option = 'EQ'
             IN sign = sign
                option = option
          ( low = ls_delivery-vgbel )
          ).

    IF l_t_delivery_reference IS NOT INITIAL.
      SORT l_t_delivery_reference.
      DELETE ADJACENT DUPLICATES FROM l_t_delivery_reference COMPARING low.

      SELECT a~vbeln, b~posnr, a~ernam, a~erdat, a~lfart, a~wadat, a~lddat,
             a~lfdat, a~kodat, a~ablad, a~kunnr, a~btgew, a~wadat_ist, a~wauhr,
             b~matnr, b~matwa, b~matkl, b~werks, b~lgort, b~charg, b~lichn,
             b~kdmat, b~prodh, b~lfimg, b~meins, b~vrkme, b~ntgew, b~brgew,
             b~gewei, b~lgmng, b~ladgr, b~tragr, b~lgnum, b~bwart, b~mtart,
             b~ean11, b~pstyv
      FROM likp AS a
      JOIN lips AS b ON
      a~vbeln = b~vbeln
      INTO TABLE @DATA(l_t_deliveries)
            WHERE a~vbeln IN @l_t_delivery_reference.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING l_t_deliveries TO mt_invoice_deliveries.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_tax_code_for_item.

    IF mt_invoice_header IS NOT INITIAL.

      "get pricing conditions to determine tax code
      SELECT knumv, kposn, mwsk1
      FROM konv
       INTO TABLE @DATA(l_t_pricing_conditions)
       FOR ALL ENTRIES IN @mt_invoice_header
                    WHERE knumv = @mt_invoice_header-knumv.
      IF sy-subrc EQ 0.
        SORT l_t_pricing_conditions BY knumv DESCENDING kposn DESCENDING.
        DELETE l_t_pricing_conditions WHERE mwsk1 IS INITIAL.
        SORT l_t_pricing_conditions BY knumv DESCENDING kposn DESCENDING.
        DELETE ADJACENT DUPLICATES FROM l_t_pricing_conditions COMPARING knumv kposn mwsk1.

        LOOP AT mt_invoice_header ASSIGNING FIELD-SYMBOL(<l_s_single_invoice>).

          LOOP AT mt_invoice_lines ASSIGNING FIELD-SYMBOL(<l_s_invoice_item>) WHERE vbeln = <l_s_single_invoice>-vbeln.

            TRY .
                <l_s_invoice_item>-mwskz = l_t_pricing_conditions[ knumv = <l_s_single_invoice>-knumv
                kposn = <l_s_invoice_item>-posnr ]-mwsk1.

              CATCH cx_sy_itab_line_not_found.
                CONTINUE.
            ENDTRY.

          ENDLOOP.

        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD generate_dmee.

    "for each invoice process DMEE
    LOOP AT mt_invoice_header ASSIGNING FIELD-SYMBOL(<l_s_invoice_header>).

      "start DME engine for invoice or keep it running if DME is already running
      start_dmee_engine( <l_s_invoice_header> ).

      "loop over each invoice item for currently processed invoice
      LOOP AT mt_invoice_lines ASSIGNING FIELD-SYMBOL(<l_s_invoice_line>) WHERE vbeln = <l_s_invoice_header>-vbeln.

        "loop over each delivery connected to invoice line or simply output invoice line if no delivery exists
        IF line_exists( mt_invoice_deliveries[ vbeln = <l_s_invoice_line>-vgbel posnr = <l_s_invoice_line>-vgpos ] ).
          LOOP AT mt_invoice_deliveries ASSIGNING FIELD-SYMBOL(<l_s_invoice_delivery>) WHERE vbeln = <l_s_invoice_line>-vgbel.
            "delivery-based-billing
            add_dmee_item( EXPORTING i_v_invoice_header = <l_s_invoice_header>
                                     i_v_invoice_line = <l_s_invoice_line>
                                     i_v_invoice_delivery = <l_s_invoice_delivery> ).
          ENDLOOP.
        ELSE.
          "order-based-billing
          add_dmee_item( EXPORTING i_v_invoice_header = <l_s_invoice_header>
                                   i_v_invoice_line = <l_s_invoice_line> ).
        ENDIF.

      ENDLOOP.
    ENDLOOP.

    "at the very end close DME engine
    close_engine( ).
    "and process display or download
    download_file( ).

  ENDMETHOD.

  METHOD start_dmee_engine.
    DATA: l_s_dmee_item        TYPE zdmee_invoice_data,
          l_t_dmee_sort_fields TYPE STANDARD TABLE OF dmee_tree_sort.

    l_s_dmee_item-zdmee_invoice_header = i_s_invoice.

    IF mv_dmee_first_started EQ abap_true.
      DATA(l_v_new_file_needed) = check_if_new_file_needed( l_s_dmee_item ).
    ENDIF.

    IF mv_dmee_first_started IS INITIAL
    OR l_v_new_file_needed IS NOT INITIAL.

      CALL FUNCTION 'DMEE_START'
        EXPORTING
          i_tree_type = lcl_tools=>c_dmee_tree_type
          i_tree_id   = m_o_screen->mv_dmee_tree
          item        = l_s_dmee_item
          param       = m_o_screen->ms_dmee_additional_data
*         uparam      = i_format_params
        TABLES
          file_output = mt_dmee_output
          sort_fields = l_t_dmee_sort_fields.

      IF mv_dmee_first_started IS INITIAL.
        mv_dmee_first_started = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD add_dmee_item.
    DATA: l_s_dmee_item TYPE zdmee_invoice_data.

    l_s_dmee_item-zdmee_invoice_header = i_v_invoice_header.
    l_s_dmee_item-zdmee_invoice_line = i_v_invoice_line.
    l_s_dmee_item-zdmee_delivery_line = i_v_invoice_delivery.

    CALL FUNCTION 'DMEE_PUT_ITEM'
      EXPORTING
        item        = l_s_dmee_item
        param       = m_o_screen->ms_dmee_additional_data
*       uparam      = params
      TABLES
*       item_tab    = tab_item
        file_output = mt_dmee_output.

  ENDMETHOD.

  METHOD close_engine.
    DATA: l_t_error_output   TYPE STANDARD TABLE OF fimsg.
    DATA: l_t_fpm_fields  TYPE STANDARD TABLE OF dmee_fpm_fields.

    CALL FUNCTION 'DMEE_END'
      TABLES
        file_output  = mt_dmee_output
        error_output = l_t_error_output
        fpm_fields   = l_t_fpm_fields.

  ENDMETHOD.

  METHOD check_if_new_file_needed.

    CALL FUNCTION 'DMEE_NEW_FILE'
      EXPORTING
        item      = i_s_dmee_data
      CHANGING
        xnew_file = r_v_new_file_needed.

    IF r_v_new_file_needed IS NOT INITIAL.

      close_engine( ).
      download_file( ).

    ENDIF.

  ENDMETHOD.

  METHOD download_file.

    CASE m_o_screen->mv_dmee_is_xml.
      WHEN abap_true.
        download_xml_file( ).
      WHEN abap_false.
        download_flat_file( ).
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD download_xml_file.
    CONSTANTS: l_c_xml TYPE string VALUE 'XML'.
    DATA:
      l_v_filename TYPE string,
      l_v_fullpath TYPE string,
      l_v_result   TYPE i,
      l_v_path     TYPE string.

    IF m_o_screen->mv_display_only EQ abap_true.

      xml_file_start_download( EXPORTING i_v_display_only = abap_true
                                         i_v_filename = l_v_filename ).
    ELSE.
      CALL METHOD cl_gui_frontend_services=>file_save_dialog
        EXPORTING
          default_extension = l_c_xml
        CHANGING
          filename          = l_v_filename
          path              = l_v_path
          fullpath          = l_v_fullpath
          user_action       = l_v_result.

      IF l_v_result IS INITIAL.

        xml_file_start_download( EXPORTING i_v_display_only = abap_false
                                           i_v_filename = l_v_filename ).

      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD download_flat_file.
    CONSTANTS c_file_txt TYPE string VALUE 'TXT'.

    DATA:
      l_v_filename TYPE string,
      l_v_path     TYPE string,
      l_v_fullpath TYPE string,
      l_v_result   TYPE i.
    DATA: l_v_codepage TYPE cpcodepage.
    DATA: l_t_file_data TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    IF m_o_screen->mv_display_only IS NOT INITIAL.

      "display only
      CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
        EXPORTING
          endpos_col   = 30
          endpos_row   = lines( mt_dmee_output )
          startpos_col = 10
          startpos_row = 1
          titletext    = text-004
*        IMPORTING
*         choise       = l_v_choice
        TABLES
          valuetab     = mt_dmee_output
        EXCEPTIONS
          break_off    = 1
          OTHERS       = 2.

    ELSE.
      "download
      LOOP AT mt_dmee_output ASSIGNING FIELD-SYMBOL(<l_s_dme_line>).

        APPEND <l_s_dme_line>-line TO l_t_file_data.

      ENDLOOP.

      "get default encoding by language
      CALL FUNCTION 'NLS_GET_FRONTEND_CP'
        EXPORTING
          langu                 = CONV spras( sy-langu ) ##OPERATOR
*         FETYPE                = 'MS'
        IMPORTING
          frontend_codepage     = l_v_codepage
        EXCEPTIONS
          illegal_syst_codepage = 1
          no_frontend_cp_found  = 2
          internal_or_db_error  = 3
          OTHERS                = 4.
      IF sy-subrc NE 0.
        l_v_codepage = '4110'.
      ENDIF.
      CONDENSE l_v_codepage NO-GAPS.

      CALL METHOD cl_gui_frontend_services=>file_save_dialog
        EXPORTING
          default_extension = CONV string( c_file_txt )
        CHANGING
          filename          = l_v_filename
          path              = l_v_path
          fullpath          = l_v_fullpath
          user_action       = l_v_result.

      IF l_v_result IS INITIAL.

        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            filename = l_v_filename
            codepage = CONV abap_encoding( l_v_codepage )
          TABLES
            data_tab = l_t_file_data
          EXCEPTIONS
            OTHERS   = 1.
        IF sy-subrc NE 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          MESSAGE i427(fr) WITH l_v_filename.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDMETHOD.

  METHOD xml_file_start_download.

    CALL FUNCTION 'DMEE_HANDLE_XML_DOC_PC'
      EXPORTING
        i_filename = i_v_filename
        i_save     = COND xfeld( WHEN i_v_display_only EQ abap_true THEN space
                                 ELSE abap_true )
        i_display  = i_v_display_only.

  ENDMETHOD.

  METHOD display_alv.

    NEW lcl_alv_display( i_t_invoice_headers = mt_invoice_header
                         i_t_invoice_lines = mt_invoice_lines
                         i_t_invoice_deliveries = mt_invoice_deliveries )->setup_alv( ).

  ENDMETHOD.

ENDCLASS.
