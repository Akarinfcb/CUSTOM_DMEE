*&---------------------------------------------------------------------*
*&  Include           Z_DMEE_INVOICE_EXTRACT_LCL00
*&---------------------------------------------------------------------*
CLASS lcl_tools DEFINITION FINAL ABSTRACT.

  PUBLIC SECTION.

    "main processing types
    TYPES: tt_invoice_header TYPE SORTED TABLE OF zdmee_invoice_header WITH UNIQUE KEY primary_key COMPONENTS vbeln.
    TYPES: tt_invoice_lines TYPE SORTED TABLE OF zdmee_invoice_line WITH UNIQUE KEY primary_key COMPONENTS vbeln posnr.
    TYPES: tt_invoice_deliveries TYPE SORTED TABLE OF zdmee_delivery_line WITH UNIQUE KEY primary_key COMPONENTS vbeln posnr.

    "ALV display is using only standard tables so need to convert data before adding to ALV
    TYPES: tt_invoice_header_alv TYPE STANDARD TABLE OF zdmee_invoice_header WITH KEY primary_key COMPONENTS vbeln.
    TYPES: tt_invoice_lines_alv TYPE STANDARD TABLE OF zdmee_invoice_line WITH KEY primary_key COMPONENTS vbeln posnr.
    TYPES: tt_invoice_deliveries_alv TYPE STANDARD TABLE OF zdmee_delivery_line WITH KEY primary_key COMPONENTS vbeln posnr.

    CONSTANTS: c_structure_header TYPE tabname VALUE 'ZDMEE_INVOICE_HEADER'.
    CONSTANTS: c_structure_line TYPE tabname VALUE 'ZDMEE_INVOICE_LINE'.
    CONSTANTS: c_structure_delivery TYPE tabname VALUE 'ZDMEE_DELIVERY_LINE'.

    CONSTANTS c_f4_var TYPE fieldname VALUE 'VARIANT'.
    CONSTANTS: c_dmee_tree_type TYPE dmee_treetype VALUE 'ZZIV'.
    CONSTANTS: c_act_disp TYPE c LENGTH 2 VALUE '03' ##NO_TEXT.

    "for selection screen options
    CLASS-DATA: l_v_sccr_sdbil TYPE vbrk-vbeln.
    CLASS-DATA: l_v_sccr_sddate TYPE fkdat.
    CLASS-DATA: l_v_sccr_sdtype TYPE rv60a-fkart.
    CLASS-DATA: l_v_sccr_sdorg TYPE vkorg.

ENDCLASS.
