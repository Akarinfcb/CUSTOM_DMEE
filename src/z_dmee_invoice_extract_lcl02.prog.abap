*&---------------------------------------------------------------------*
*&  Include           Z_DMEE_INVOICE_EXTRACT_LCL02
*&---------------------------------------------------------------------*
CLASS lcl_alv_display DEFINITION FINAL CREATE PUBLIC.


  PUBLIC SECTION.
    METHODS: setup_alv.

    METHODS: constructor IMPORTING i_t_invoice_headers    TYPE lcl_tools=>tt_invoice_header
                                   i_t_invoice_lines      TYPE lcl_tools=>tt_invoice_lines
                                   i_t_invoice_deliveries TYPE lcl_tools=>tt_invoice_deliveries.

  PRIVATE SECTION.
    CLASS-DATA m_o_screen TYPE REF TO lcl_screen.
    DATA: mo_alv_transformer TYPE REF TO cl_xx_alv_transformer.
    DATA: mo_alv_display TYPE REF TO cl_xx_alv_display.
    DATA: mt_headers TYPE lcl_tools=>tt_invoice_header.
    DATA: mt_lines TYPE lcl_tools=>tt_invoice_lines.
    DATA: mt_deliveries TYPE lcl_tools=>tt_invoice_deliveries.

    "ALV display
    METHODS:
      setup_alv_nodes,
      display.

ENDCLASS.

CLASS lcl_alv_display IMPLEMENTATION.

  METHOD constructor.
    m_o_screen = lcl_screen=>get_instance( ).
    mo_alv_transformer = NEW cl_xx_alv_transformer( ).
    mo_alv_display = NEW cl_xx_alv_display( ).
    mt_headers = i_t_invoice_headers.
    mt_lines = i_t_invoice_lines.
    mt_deliveries = i_t_invoice_deliveries.

  ENDMETHOD.

  METHOD setup_alv.

    setup_alv_nodes( ).
    display( ).

  ENDMETHOD.

  METHOD setup_alv_nodes.
    DATA: l_v_header_title TYPE sytitle.
    DATA: l_v_title        TYPE char30.

    l_v_header_title = text-001.

    "inform user what is happening now
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = text-i01.

    TRY.
        "add single invoice ALV header
        mo_alv_transformer->add_alv_view( iv_is_header = abap_true
                                          iv_header_title  = l_v_header_title ).

        "Folder for each invoice
        LOOP AT mt_headers ASSIGNING FIELD-SYMBOL(<l_s_invoice_header>).
          WRITE <l_s_invoice_header>-vbeln TO l_v_title.

          DATA(l_t_headers_alv) = VALUE lcl_tools=>tt_invoice_header_alv( ( <l_s_invoice_header> ) ).

          mo_alv_transformer->add_alv_view( iv_structure_name = lcl_tools=>c_structure_header
                                            iv_default_variant = m_o_screen->mv_layout
                                            it_outtab   = l_t_headers_alv
                                            iv_is_tree_folder = abap_true
                                            iv_tree_folder_title = l_v_title ).

          LOOP AT mt_lines ASSIGNING FIELD-SYMBOL(<l_s_invoice_line>) WHERE vbeln = <l_s_invoice_header>-vbeln.

            DATA(l_t_lines_alv) = VALUE lcl_tools=>tt_invoice_lines_alv( ( <l_s_invoice_line> ) ).

            l_v_title = |{ <l_s_invoice_line>-posnr ALPHA = OUT }|.

            "If delivery exists for a line - make a folder
            "If not - just output items as subnodes
            IF line_exists( mt_deliveries[ vbeln = <l_s_invoice_line>-vgbel ] ).
              DATA(l_v_delivery_exists) = abap_true.
            ELSE.
              CLEAR l_v_delivery_exists.
            ENDIF.

            mo_alv_transformer->add_alv_view( iv_structure_name = lcl_tools=>c_structure_line
                                              it_outtab = l_t_lines_alv
                                              iv_is_tree_folder = COND xfeld( WHEN l_v_delivery_exists EQ abap_true THEN abap_true
                                                                              ELSE abap_false )
                                              iv_tree_folder_title =  COND char30( WHEN l_v_delivery_exists EQ abap_true THEN l_v_title
                                                                                   ELSE space )
                                              iv_is_tree = COND xfeld( WHEN l_v_delivery_exists EQ abap_true  THEN abap_false
                                                                       ELSE abap_true )
                                              iv_tree_title = COND char30( WHEN l_v_delivery_exists EQ abap_true  THEN space
                                                                           ELSE l_v_title )
                                              iv_tree_level = COND int2( WHEN l_v_delivery_exists EQ abap_true  THEN 1
                                                                         ELSE 0 )
                                              iv_is_tabstrip = abap_true
                                              ).

            LOOP AT mt_deliveries ASSIGNING FIELD-SYMBOL(<l_s_invoice_delivery>) WHERE vbeln = <l_s_invoice_line>-vgbel AND posnr = <l_s_invoice_line>-vgpos.

              DATA(l_t_deliveries_alv) = VALUE lcl_tools=>tt_invoice_deliveries_alv( ( <l_s_invoice_delivery> ) ).

              l_v_title = |{ <l_s_invoice_delivery>-vbeln ALPHA = OUT }{ <l_s_invoice_delivery>-posnr ALPHA = OUT }|.

              "If deliveries exist - output them as subnodes of items
              mo_alv_transformer->add_alv_view( iv_structure_name = lcl_tools=>c_structure_delivery
                                                it_outtab = l_t_deliveries_alv
                                                iv_tree_title = l_v_title
                                                iv_is_tree = abap_true
                                                iv_tree_level = 1 ).

            ENDLOOP.

          ENDLOOP.
        ENDLOOP.

      CATCH cx_xx_shared_exception.                     "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.

  METHOD display.

    TRY .
        "Adding of this class as ALV transformer module
        mo_alv_display->add_alv_transformer( mo_alv_transformer ).

        "Displays ALV
        mo_alv_display->display_alv( ).

      CATCH cx_xx_shared_exception.                     "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
