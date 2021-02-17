REPORT z_dmee_invoice_extract.

"Custom DMEE Tree Type handling
INCLUDE z_dmee_invoice_extract_lcl00."Tools and data types
INCLUDE z_dmee_invoice_extract_ssc.  "Selection Screen
INCLUDE z_dmee_invoice_extract_lcl01."Screen processing class
INCLUDE z_dmee_invoice_extract_lcl02."ALV processing
INCLUDE z_dmee_invoice_extract_lcl03."Main report processing

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  lcl_screen=>call_f4( lcl_tools=>c_f4_var ).

START-OF-SELECTION.
  lcl_screen=>get_instance( )->initialize( ).
  lcl_report=>get_instance( )->process( ).
