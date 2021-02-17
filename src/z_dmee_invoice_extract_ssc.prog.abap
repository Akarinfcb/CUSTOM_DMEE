*&---------------------------------------------------------------------*
*&  Include           Z_DMEE_INVOICE_EXTRACT_SSC
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-fb1.

PARAMETERS: p_comp TYPE bukrs OBLIGATORY DEFAULT '1000'.

SELECT-OPTIONS: so_sddoc FOR lcl_tools=>l_v_sccr_sdbil.
SELECT-OPTIONS: so_sddat FOR lcl_tools=>l_v_sccr_sddate.

SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-fb2.

SELECT-OPTIONS: so_sdorg FOR lcl_tools=>l_v_sccr_sdorg.
SELECT-OPTIONS: so_sdtyp FOR lcl_tools=>l_v_sccr_sdtype.

SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-fb3.

PARAMETERS: p_disp TYPE xtest DEFAULT 'X'.
PARAMETERS: tree_typ TYPE dmee_tree_head-tree_type DEFAULT lcl_tools=>c_dmee_tree_type NO-DISPLAY.
PARAMETERS: p_tree TYPE dmee_tree_head-tree_id OBLIGATORY.
PARAMETERS: p_vari TYPE slis_vari.

SELECTION-SCREEN END OF BLOCK b03.
