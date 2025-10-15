
*  Move imported texts to global variables
PERFORM read_text_fields
  TABLES   it_text_fields
  CHANGING gv_service_type
           gv_service_taker
           gv_cfop_text
           gv_transp_mode
           gv_strt_jcd
           gv_end_jcd
           gv_icms_taxsit.


* Assign short description of CT-e type
PERFORM assign_cte_type
  USING     is_additional_fields-cte_type_xml
  CHANGING  gv_cte_type.


* Shorten CFOP to four digits
PERFORM shorten_cfop_to_4_digits
  USING     is_lin-cfop
  CHANGING  gv_cfop.


* Access key format: after 4 digits enter a separator (.)
PERFORM format_access_key
  USING     is_additional_fields-access_key
  CHANGING  gv_access_key_string.


* Assign short description of start/end region
PERFORM assign_start_and_end_region
  USING    is_doc
  CHANGING gv_strt_region
           gv_end_region.


* Read tax values for ICMS/ICST
PERFORM prepare_taxes_for_display
  TABLES   it_tax
  USING    is_lin
  CHANGING gv_icms_base
           gv_icms_rate
           gv_icms_taxval
           gv_icms_basered
           gv_icms_st_taxval.


* Assign totals
PERFORM assign_nfnet_and_nftot
  USING     is_additional_fields
  CHANGING  gv_nfnet
            gv_nftot.


* Fill tables of origin documents in main window
PERFORM fill_origin_doc_tab
  TABLES    it_doc_ref
  USING     is_service_taker
  CHANGING  gt_origin_doc.
