*&---------------------------------------------------------------------*
*&  Include           Z_DMEE_INVOICE_EXTRACT_LCL01
*&---------------------------------------------------------------------*
CLASS lcl_screen DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS get_instance RETURNING VALUE(r_o_screen) TYPE REF TO lcl_screen.
    CLASS-METHODS call_f4 IMPORTING i_v_field TYPE fieldname.
    CLASS-METHODS ask_for_variants.
    METHODS: initialize. "saves all selection-screen parameters for internal processing

    CLASS-DATA: mt_billing_docs TYPE RANGE OF vbeln_vf.
    CLASS-DATA: mt_billing_date TYPE RANGE OF fkdat.
    CLASS-DATA: mt_billing_types TYPE RANGE OF fkart.
    CLASS-DATA: mt_sales_org TYPE RANGE OF vkorg.
    CLASS-DATA: mv_dmee_tree TYPE dmee_treeid.
    CLASS-DATA: ms_dmee_additional_data TYPE zdmee_invoice_add_data.
    CLASS-DATA: mv_company_code TYPE bukrs.
    CLASS-DATA: mv_dmee_is_xml TYPE xfeld.
    CLASS-DATA: mv_display_only TYPE xfeld.
    CLASS-DATA: mv_layout TYPE char12.

  PRIVATE SECTION.
    CLASS-DATA m_o_screen TYPE REF TO lcl_screen.

    METHODS auth_company_code_single IMPORTING VALUE(i_v_company_code) TYPE bukrs
                                     RETURNING VALUE(r_v_authority_ok) TYPE boolean.
    METHODS get_additional_dme_params RETURNING VALUE(r_s_additional_data) TYPE zdmee_invoice_add_data.
    METHODS check_dmee_format RETURNING VALUE(r_v_is_xml) TYPE xfeld.

ENDCLASS.

CLASS lcl_screen IMPLEMENTATION.

  METHOD get_instance.

    IF m_o_screen IS INITIAL.
      CREATE OBJECT m_o_screen.
    ENDIF.
    r_o_screen = m_o_screen.

  ENDMETHOD.

  METHOD call_f4.

    CASE i_v_field.
      WHEN lcl_tools=>c_f4_var.

        ask_for_variants( ).

      WHEN OTHERS.
        "do nothing
    ENDCASE.

  ENDMETHOD.

  METHOD ask_for_variants.
    DATA: l_v_exit TYPE c LENGTH 1.
    DATA: l_s_layout TYPE disvariant.

    "in case of ALV used in this report it will always be structure name + report name
    l_s_layout-report = |{ lcl_tools=>c_structure_header }{ sy-repid }|.
    l_s_layout-username = sy-uname.
    CALL FUNCTION 'LT_F4_FOR_VARIANTS'
      EXPORTING
        is_variant         = l_s_layout
        i_display_via_grid = 'X'
      IMPORTING
        e_exit             = l_v_exit
        es_variant         = l_s_layout
      EXCEPTIONS
        not_found          = 1
        OTHERS             = 2.
    IF sy-subrc NE 0.
      MESSAGE text-002 TYPE 'S'. "no layout found
    ELSE.
      IF l_v_exit IS INITIAL.
        p_vari = l_s_layout-variant.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD initialize.

    IF auth_company_code_single( p_comp ) NE abap_true.
      MESSAGE e060(f0) WITH p_comp.
    ENDIF.
    me->mv_company_code = p_comp.
    me->mt_billing_docs = so_sddoc[].
    me->mt_billing_date = so_sddat[].
    me->mt_billing_types = so_sdtyp[].
    me->mt_sales_org = so_sdorg[].
    me->mv_dmee_tree = p_tree.
    me->mv_display_only = p_disp.
    me->mv_layout = p_vari.
    me->ms_dmee_additional_data = get_additional_dme_params( ).
    me->mv_dmee_is_xml = check_dmee_format( ).

  ENDMETHOD.

  METHOD auth_company_code_single.

    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
    ID 'BUKRS' FIELD i_v_company_code
    ID 'ACTVT' FIELD lcl_tools=>c_act_disp.
    IF sy-subrc EQ 0.
      r_v_authority_ok  = abap_true.
    ELSE.
      r_v_authority_ok = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD get_additional_dme_params.
    DATA: l_s_company_address TYPE szadr_addr1_complete.

    SELECT SINGLE bukrs butxt ort01 land1 waers stceg adrnr
      FROM t001
      INTO r_s_additional_data
      WHERE bukrs = mv_company_code.

    CALL FUNCTION 'ADDR_GET_COMPLETE'
      EXPORTING
        addrnumber              = r_s_additional_data-adrnr
      IMPORTING
        addr1_complete          = l_s_company_address
      EXCEPTIONS
        parameter_error         = 1
        address_not_exist       = 2
        internal_error          = 3
        wrong_access_to_archive = 4
        address_blocked         = 5
        OTHERS                  = 6.
    IF sy-subrc EQ 0.
      READ TABLE l_s_company_address-addr1_tab ASSIGNING FIELD-SYMBOL(<l_s_company_address>) INDEX 1.
      IF sy-subrc EQ 0.
        r_s_additional_data-post_code1 = <l_s_company_address>-data-post_code1.
        r_s_additional_data-street = <l_s_company_address>-data-street.
        r_s_additional_data-house_num1 = <l_s_company_address>-data-house_num1.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD check_dmee_format.

    SELECT SINGLE xml_tree
      FROM  dmee_tree
      INTO r_v_is_xml
      WHERE tree_type = lcl_tools=>c_dmee_tree_type
      AND tree_id = mv_dmee_tree.

  ENDMETHOD.

ENDCLASS.
