FUNCTION zbapi_goodsmvt_getitems .                          "#EC ENHOK
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_SELEC_DIRETO) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      MATERIAL_RA STRUCTURE  BAPI2017_GM_MATERIAL_RA OPTIONAL
*"      PLANT_RA STRUCTURE  BAPI2017_GM_PLANT_RA OPTIONAL
*"      STGE_LOC_RA STRUCTURE  BAPI2017_GM_STGE_LOC_RA OPTIONAL
*"      BATCH_RA STRUCTURE  BAPI2017_GM_BATCH_RA OPTIONAL
*"      MOVE_TYPE_RA STRUCTURE  BAPI2017_GM_MOVE_TYPE_RA OPTIONAL
*"      SPEC_STOCK_RA STRUCTURE  BAPI2017_GM_SPEC_STOCK_RA OPTIONAL
*"      TR_EV_TYPE_RA STRUCTURE  BAPI2017_GM_TR_EV_TYPE_RA OPTIONAL
*"      PSTNG_DATE_RA STRUCTURE  BAPI2017_GM_PSTNG_DATE_RA OPTIONAL
*"      VENDOR_RA STRUCTURE  BAPI2017_GM_VENDOR_RA OPTIONAL
*"      USERNAME_RA STRUCTURE  BAPI2017_GM_USERNAME_RA OPTIONAL
*"      PURCH_DOC_RA STRUCTURE  BAPI2017_GM_PURCH_DOC_RA OPTIONAL
*"      GOODSMVT_HEADER STRUCTURE  BAPI2017_GM_HEAD_02
*"      GOODSMVT_ITEMS STRUCTURE  BAPI2017_GM_ITEM_SHOW
*"      RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------
*                                                           "n1103143
* Oct. 2007 MM : performance improved                       "n1103143
* Improvements :                                            "n1103143
* --------------                                            "n1103143
* - the database selection MM docs will be carried out in   "n1103143
*   3 steps :                                               "n1103143
*     1. read the keys of the MM docs considerung the       "n1103143
*        restriction, and check authority                   "n1103143
*     2. read all matching MM doc items from table MSEG     "n1103143
*     3. read all matching MM doc heades from table MKPF    "n1103143
* - only one header entry will be written into the output   "n1103143
*   table GOODSMVT_HEADER for one MM doc                    "n1103143
* - the additional MESSAGE commands makes sure to find this "n1103143
*   source code in the where-used list of the error message "n1103143
*   using transaction SE91                                  "n1103143
* - carry out the authority checks in a method with buffer  "n1103143
*                                                           "n1103143

*ENHANCEMENT-POINT BAPI_GOODSMVT_GETITEMS_G8 SPOTS ES_SAPLMB_BUS2017 STATIC.

*ENHANCEMENT-POINT BAPI_GOODSMVT_GETITEMS_G6 SPOTS ES_SAPLMB_BUS2017.


  TYPES : BEGIN OF ty_mkpf,
            mblnr TYPE  mkpf-mblnr,
            mjahr TYPE  mkpf-mjahr.
  TYPES : END  OF ty_mkpf.

  DATA: BEGIN OF i_mkpf OCCURS 0.
          INCLUDE STRUCTURE mkpf.
  DATA: END OF i_mkpf.

  DATA: BEGIN OF i_mseg OCCURS 0.
          INCLUDE STRUCTURE mseg.
  DATA: END OF i_mseg.

  DATA: t_mkpf   TYPE TABLE OF ty_mkpf.

*****REDESIGN FOR LAMA.
  RANGES: mgv_material_ra FOR mara-matnr.

*----------------------------------------------------------------------*

  TYPE-POOLS : abap.

* working fields for checking the plants
  DATA : lt_t001w            TYPE  STANDARD TABLE OF t001w.
  FIELD-SYMBOLS :
         <ls_t001w>          TYPE  t001w.

  DATA : lt_werks_ra TYPE  STANDARD TABLE OF bapi2017_gm_plant_ra,
         ls_werks_ra LIKE  LINE OF lt_werks_ra.
  FIELD-SYMBOLS :
         <ls_plant_ra>       LIKE  LINE OF plant_ra.

  DATA : lv_cnt_plants          TYPE  i,
         lv_flag_check_plants   TYPE  abap_bool,
         lv_flag_check_plants_2 TYPE  abap_bool,
         lv_flag_check_bwart_2  TYPE  abap_bool,
         lv_flag_auth_error     TYPE  abap_bool.

* working fields for checking the storage locations
  DATA : lt_t001l            TYPE  STANDARD TABLE OF t001l. "#EC NEEDED
  FIELD-SYMBOLS :
         <ls_move_type_ra>   LIKE  LINE OF move_type_ra.

* working fields for checking the movement type
  DATA : lt_t156             TYPE  STANDARD TABLE OF t156.  "#EC NEEDED
  FIELD-SYMBOLS :
         <ls_stge_loc_ra>    LIKE  LINE OF stge_loc_ra.

* working fields for checking the special stock indicator
  DATA : lt_t148             TYPE  STANDARD TABLE OF t148.  "#EC NEEDED
  FIELD-SYMBOLS :
         <ls_spec_stock_ra>  LIKE  LINE OF spec_stock_ra.

* working fields for checking the Transaction/Event Type
  DATA : lt_t158v            TYPE  STANDARD TABLE OF t158v. "#EC NEEDED
  FIELD-SYMBOLS :
         <ls_tr_ev_type_ra>  LIKE  LINE OF tr_ev_type_ra.

* working areas for processing the keys of the matching MM doc items
  TYPES : BEGIN OF stype_mm_docs,
            mblnr TYPE  mkpf-mblnr,
            mjahr TYPE  mkpf-mjahr,
            zeile TYPE  mseg-zeile,
            werks TYPE  mseg-werks,
            bwart TYPE  mseg-bwart,
*
            matnr TYPE  mseg-matnr,
            charg TYPE  mseg-charg,
            lgort TYPE  mseg-lgort,
            sobkz TYPE  mseg-sobkz,
            lifnr TYPE  mseg-lifnr,
            ebeln TYPE  mseg-ebeln,
          END OF stype_mm_docs,

          stab_mm_docs TYPE  STANDARD TABLE OF stype_mm_docs.

  DATA : lt_mm_docs          TYPE  stab_mm_docs.
  FIELD-SYMBOLS :
         <ls_mm_docs>        TYPE  stype_mm_docs.

* working areas key for reading MM doc headers
  TYPES : BEGIN OF stype_mkpf_key,
            mblnr TYPE  mkpf-mblnr,
            mjahr TYPE  mkpf-mjahr,
          END OF stype_mkpf_key,

          stab_mkpf_key TYPE  STANDARD TABLE OF stype_mkpf_key.

  DATA : ls_mkpf_key_old TYPE  stype_mkpf_key,
         ls_mkpf_key     TYPE  stype_mkpf_key,
         lt_mkpf_key     TYPE  stab_mkpf_key.

* working table for the fields to be transported
  DATA : lt_fields           TYPE  STANDARD TABLE OF fieldname.

* structure for the error messages
  DATA : ls_return           TYPE  bapiret2.

* flag for MSEG conversion - if done, flag is X, else space    "n1887715
  DATA lv_mseg_conv_done(1) TYPE c.                         "n1887715
  FIELD-SYMBOLS :                                           "n1887715
       <ls_i_mseg>          TYPE    mseg.                   "n1887715
*----------------------------------------------------------------------*

* create table with the fields to be transported in the first step
  APPEND  'MSEG~MBLNR'       TO  lt_fields.
  APPEND  'MSEG~MJAHR'       TO  lt_fields.
  APPEND  'MSEG~ZEILE'       TO  lt_fields.
  APPEND  'MSEG~BWART'       TO  lt_fields.

*----------------------------------------------------------------------*

  CLEAR return. REFRESH return.
  mgv_material_ra[] = material_ra[].

  PERFORM check_ranges: TABLES mgv_material_ra   return,
                        TABLES plant_ra          return,
                        TABLES stge_loc_ra       return,
                        TABLES batch_ra          return,
                        TABLES move_type_ra      return,
                        TABLES spec_stock_ra     return,
                        TABLES tr_ev_type_ra     return,
                        TABLES pstng_date_ra     return,
                        TABLES vendor_ra         return,
                        TABLES username_ra       return,
                        TABLES purch_doc_ra      return.

*----------------------------------------------------------------------*
* check restrictions for plant
*----------------------------------------------------------------------*
  SELECT werks               FROM t001w
    INTO CORRESPONDING FIELDS OF TABLE lt_t001w
        WHERE werks IN plant_ra.

  IF NOT sy-subrc IS INITIAL.
*   no matching plants found
    LOOP AT plant_ra         ASSIGNING  <ls_plant_ra>.
      MOVE <ls_plant_ra>-low TO sy-msgv1.
      EXIT.
    ENDLOOP.

*   Plant & does not exist
    MESSAGE e102(m3)         WITH  <ls_plant_ra>-low
                             INTO  ls_return-message.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type      = 'E'
        cl        = 'M3'
        number    = '102'
        par1      = sy-msgv1
*       PAR2      = ' '
*       PAR3      = ' '
*       PAR4      = ' '
*       LOG_NO    = ' '
*       LOG_MSG_NO = ' '
        parameter = ret_parameter
        row       = ret_row
        field     = ret_field
      IMPORTING
        return    = ls_return.
    APPEND  ls_return       TO  return.
  ENDIF.

* check whether the user has authority for the found plants
  LOOP AT lt_t001w           ASSIGNING <ls_t001w>.
*   Authorization for material documents plant level
    PERFORM  check_authorities
                             TABLES    return
                             USING     abap_true       " check plants
                                       <ls_t001w>-werks
                                       abap_false      " no movement types
                                       space
                             CHANGING  lv_flag_auth_error.

    IF  lv_flag_auth_error = abap_false.
*     ok -> fill new range table
      MOVE : 'I'                  TO  ls_werks_ra-sign,
             'EQ'                 TO  ls_werks_ra-option,
             <ls_t001w>-werks     TO  ls_werks_ra-low.
      APPEND ls_werks_ra          TO  lt_werks_ra.
      ADD    1                    TO  lv_cnt_plants.
    ENDIF.
  ENDLOOP.

* if there are no or more than 50 plant to be selected, take the
* input restrictions and carry out the authority check for the
* plants later
  IF  lv_cnt_plants =  0  OR
      lv_cnt_plants > 50.
    MOVE : abap_true          TO  lv_flag_check_plants,
           abap_true          TO  lv_flag_check_plants_2,
           plant_ra[]         TO  lt_werks_ra[].
    APPEND  'MSEG~WERKS'      TO  lt_fields.
  ENDIF.

  MOVE : abap_true            TO  lv_flag_check_bwart_2.

*----------------------------------------------------------------------*
* check storage location
*----------------------------------------------------------------------*
  IF NOT stge_loc_ra[] IS INITIAL.
    SELECT lgort             FROM t001l                 "#EC CI_GENBUFF
      INTO CORRESPONDING FIELDS OF TABLE lt_t001l
      UP TO 1 ROWS
        WHERE  werks IN plant_ra
          AND  lgort IN stge_loc_ra.

    IF NOT sy-subrc IS INITIAL.
*     no matching storage location found
      LOOP AT stge_loc_ra              ASSIGNING  <ls_stge_loc_ra>.
        MOVE  <ls_stge_loc_ra>-low     TO  sy-msgv1.
        EXIT.
      ENDLOOP.

      LOOP AT plant_ra                 ASSIGNING  <ls_plant_ra>.
        MOVE  <ls_plant_ra>-low        TO  sy-msgv2.
        EXIT.
      ENDLOOP.

*     Storage location & does not exist in plant &
      MESSAGE e103(m3)       WITH  <ls_stge_loc_ra>-low
                                   <ls_plant_ra>-low
                             INTO  ls_return-message.

      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type      = 'E'
          cl        = 'M3'
          number    = '103'
          par1      = sy-msgv1
          par2      = sy-msgv2
*         PAR3      = ' '
*         PAR4      = ' '
*         LOG_NO    = ' '
*         LOG_MSG_NO = ' '
          parameter = ret_parameter
          row       = ret_row
          field     = ret_field
        IMPORTING
          return    = ls_return.
      APPEND  ls_return      TO  return.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* check restriction for movement type
*----------------------------------------------------------------------*
  IF NOT move_type_ra[] IS INITIAL.
    SELECT bwart             FROM  t156
      INTO CORRESPONDING FIELDS OF TABLE lt_t156
        UP TO 1 ROWS
          WHERE bwart IN move_type_ra.

    IF NOT sy-subrc IS INITIAL.
      LOOP AT move_type_ra             ASSIGNING  <ls_move_type_ra>.
        MOVE  <ls_move_type_ra>-low    TO  sy-msgv1.
        EXIT.
      ENDLOOP.

*     Movement type does not exist
      MESSAGE e220(m7)       INTO  ls_return-message.

      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type      = 'E'
          cl        = 'M7'
          number    = '220'
          par1      = sy-msgv1
*         par2      = ' '
*         PAR3      = ' '
*         PAR4      = ' '
*         LOG_NO    = ' '
*         LOG_MSG_NO = ' '
          parameter = ret_parameter
          row       = ret_row
          field     = ret_field
        IMPORTING
          return    = ls_return.

*     insert the entered movement type into the text field
      CONCATENATE   ls_return-message  <ls_move_type_ra>-low
                             INTO  ls_return-message
                             SEPARATED BY space.
      APPEND ls_return       TO  return.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* check restriction for special stock indicator
*----------------------------------------------------------------------*
  IF NOT spec_stock_ra[] IS INITIAL.
    SELECT  sobkz            FROM  t148
      INTO CORRESPONDING FIELDS OF TABLE lt_t148
        UP TO 1 ROWS
          WHERE sobkz IN spec_stock_ra.

    IF NOT sy-subrc IS INITIAL.
      LOOP AT spec_stock_ra            ASSIGNING  <ls_spec_stock_ra>.
        MOVE  <ls_spec_stock_ra>-low   TO  sy-msgv1.
        EXIT.
      ENDLOOP.

*     Special stock indicator do not exist
      MESSAGE  e221(m7)      INTO  ls_return-message.

      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type      = 'E'
          cl        = 'M7'
          number    = '221'
          par1      = sy-msgv1
*         par2      = ' '
*         PAR3      = ' '
*         PAR4      = ' '
*         LOG_NO    = ' '
*         LOG_MSG_NO = ' '
          parameter = ret_parameter
          row       = ret_row
          field     = ret_field
        IMPORTING
          return    = ls_return.

*     add the entered special stock indicator the text field
      CONCATENATE ls_return-message <ls_spec_stock_ra>-low
                             INTO  ls_return-message
                             SEPARATED BY space.
      APPEND ls_return       TO  return.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* check restriction for Transaction/Event Type
*----------------------------------------------------------------------*
  IF NOT tr_ev_type_ra[] IS INITIAL.
    SELECT  vgart            FROM  t158v
      INTO CORRESPONDING FIELDS OF TABLE lt_t158v
        UP TO 1 ROWS
          WHERE vgart IN tr_ev_type_ra.

    IF NOT sy-subrc IS INITIAL.
      LOOP AT tr_ev_type_ra            ASSIGNING <ls_tr_ev_type_ra>.
        MOVE <ls_tr_ev_type_ra>-low    TO  sy-msgv1.
        EXIT.
      ENDLOOP.

*     Transaction/event type & does not exist
      MESSAGE  e888(m7)      WITH  <ls_tr_ev_type_ra>-low
                             INTO  ls_return-message.

      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type      = 'E'
          cl        = 'M7'
          number    = '888'
          par1      = sy-msgv1
*         par2      = ' '
*         PAR3      = ' '
*         PAR4      = ' '
*         LOG_NO    = ' '
*         LOG_MSG_NO = ' '
          parameter = ret_parameter
          row       = ret_row
          field     = ret_field
        IMPORTING
          return    = ls_return.

      APPEND ls_return       TO  return.
    ENDIF.
  ENDIF.


*ENHANCEMENT-POINT BAPI_GOODSMVT_GETITEMS_01 SPOTS ES_SAPLMB_BUS2017.

*ENHANCEMENT-SECTION     BAPI_GOODSMVT_GETITEMS_02 SPOTS ES_SAPLMB_BUS2017.

*----------------------------------------------------------------------*
* in this standard coding of this ENHANCEMENT-SECTION the MM doc items
* MSEG will be selected. This will be carried out in 3 steps if the
* MSEG conversion was not yet done
*----------------------------------------------------------------------*
* was the MSEG conversion already done?                        "n1887715
  CALL FUNCTION 'MB_CHECK_MSEG_CONVERSION_DONE'                "n1887715
    IMPORTING                                                   "n1887715
      e_conversion_done = lv_mseg_conv_done.              "n1887715
                                                            "n1887715
  IF lv_mseg_conv_done IS INITIAL.                          "n1887715
*  process the original logic before MSEG conversion was done with the
*  following 3 steps:
* 1. select the key fields form the MM doc items and headers using a
*    inner join which considers both tables MKPF and MSEG
* 2. carry out the authority checks
* 3. read all fields of the matching MM doc items into table I_MSEG
*
*----------------------------------------------------------------------*
* first step : select the keys of the matching MM docs items and the
* fields for the authority checks
*----------------------------------------------------------------------*
*-CS2020000758 - 31.03.2021 - JT - inicio
    SELECT mblnr mjahr
      FROM mkpf
      INTO TABLE t_mkpf
     WHERE budat IN pstng_date_ra
       AND vgart IN tr_ev_type_ra
       AND usnam IN username_ra.

    IF t_mkpf[] IS NOT INITIAL.
      IF i_selec_direto = abap_true.
        SELECT mblnr mjahr zeile werks
               bwart matnr charg lgort
               sobkz lifnr ebeln
          INTO TABLE lt_mm_docs
          FROM mseg
           FOR ALL ENTRIES IN t_mkpf
         WHERE mblnr  = t_mkpf-mblnr
           AND mjahr  = t_mkpf-mjahr.

        DELETE lt_mm_docs WHERE matnr NOT IN mgv_material_ra.
        DELETE lt_mm_docs WHERE charg NOT IN batch_ra.
        DELETE lt_mm_docs WHERE werks NOT IN lt_werks_ra.
        DELETE lt_mm_docs WHERE lgort NOT IN stge_loc_ra.
        DELETE lt_mm_docs WHERE bwart NOT IN move_type_ra.
        DELETE lt_mm_docs WHERE sobkz NOT IN spec_stock_ra.
        DELETE lt_mm_docs WHERE lifnr NOT IN vendor_ra.
        DELETE lt_mm_docs WHERE ebeln NOT IN purch_doc_ra.
      ELSE.
        SELECT (lt_fields)
          INTO CORRESPONDING FIELDS OF TABLE lt_mm_docs
          FROM mseg
           FOR ALL ENTRIES IN t_mkpf
         WHERE mblnr  = t_mkpf-mblnr
           AND mjahr  = t_mkpf-mjahr
           AND matnr IN mgv_material_ra
           AND charg IN batch_ra
           AND werks IN lt_werks_ra
           AND lgort IN stge_loc_ra
           AND bwart IN move_type_ra
           AND sobkz IN spec_stock_ra
           AND lifnr IN vendor_ra
           AND ebeln IN purch_doc_ra.
      ENDIF.
    ENDIF.
*-CS2020000758 - 31.03.2021 - JT - fim

*----------------------------------------------------------------------*
* second step : check whether the user has the requireds authorizations
*----------------------------------------------------------------------*
*   IF  sy-subrc IS INITIAL.
    IF  lt_mm_docs[] IS NOT INITIAL.
*   do not check the authorities during processing table I_MSEG
      MOVE : abap_false        TO  lv_flag_check_plants_2,
             abap_false        TO  lv_flag_check_bwart_2.

      LOOP AT lt_mm_docs       ASSIGNING  <ls_mm_docs>.
*       carry out the authority checks for plant if necessary and always
*       for movement type
        PERFORM  check_authorities
                               TABLES    return
                               USING     lv_flag_check_plants " plants ?
                                         <ls_mm_docs>-werks
                                         abap_true            " mvt type
                                         <ls_mm_docs>-bwart
                               CHANGING  lv_flag_auth_error.

        IF  lv_flag_auth_error = abap_true.
          DELETE              lt_mm_docs.
        ENDIF.
      ENDLOOP.

*  ----------------------------------------------------------------------*
*     third step : read the MM doc items using the remaining keys
*  ----------------------------------------------------------------------*
      IF  NOT lt_mm_docs[] IS INITIAL.
*       key table is filled / read MM docs with all fields
        SELECT *                 FROM  mseg
          INTO CORRESPONDING FIELDS OF TABLE i_mseg
            FOR ALL ENTRIES IN lt_mm_docs
              WHERE mblnr = lt_mm_docs-mblnr
                AND mjahr = lt_mm_docs-mjahr
                AND zeile = lt_mm_docs-zeile.
      ENDIF.
    ENDIF.
  ELSE.                                                     "n1887715
*  MSEG conversion was done - process new logic (faster)       "n1887715
*  >>> Start of new selection logic                            "n1887715
*----------------------------------------------------------------------*
* first step : select the matching MM docs items
*----------------------------------------------------------------------*
    SELECT *
            INTO CORRESPONDING FIELDS OF TABLE i_mseg
                    FROM mseg
                    WHERE mseg~budat_mkpf IN pstng_date_ra
                      AND mseg~vgart_mkpf IN tr_ev_type_ra
                      AND mseg~usnam_mkpf IN username_ra
                      AND mseg~matnr      IN mgv_material_ra
                      AND mseg~werks      IN lt_werks_ra
                      AND mseg~lgort      IN stge_loc_ra
                      AND mseg~charg      IN batch_ra
                      AND mseg~bwart      IN move_type_ra
                      AND mseg~sobkz      IN spec_stock_ra
                      AND mseg~lifnr      IN vendor_ra
                      AND mseg~ebeln      IN purch_doc_ra.

*  ----------------------------------------------------------------------*
*   second step : check whether the user has the requireds authorizations
*  ----------------------------------------------------------------------*
    IF  sy-subrc IS INITIAL.
*     do not check the authorities during processing table I_MSEG
      MOVE : abap_false        TO  lv_flag_check_plants_2,
             abap_false        TO  lv_flag_check_bwart_2.

      LOOP AT i_mseg           ASSIGNING  <ls_i_mseg>.
*       carry out the authority checks for plant if necessary and always
*       for movement type
        PERFORM  check_authorities
                               TABLES    return
                               USING     lv_flag_check_plants " plants ?
                                         <ls_i_mseg>-werks
                                         abap_true            " mvt type
                                         <ls_i_mseg>-bwart
                               CHANGING  lv_flag_auth_error.

        IF  lv_flag_auth_error = abap_true.
          DELETE              i_mseg.
        ENDIF.
      ENDLOOP.
    ENDIF.
*  <<< End of new selection logic                              "n1887715
  ENDIF.                                                    "n1887715

*END-ENHANCEMENT-SECTION.

************************************************************************
*                                                                      *
* After this ENHANCEMENT-SECTION the internal table I_MSEG is filled   *
* when matching MM doc items were found.                               *
* If table I_MSEG has been filled in the aboved coding the authority   *
* checks are already done.                                             *
* If the table I_MSEG is filled in an other coding carried out by the  *
* ENHANCEMENT framework the authority checks have to be done here.     *
*                                                                      *
************************************************************************

  SORT i_mseg                BY mblnr mjahr zeile.

  READ TABLE i_mseg INDEX 1.

  IF NOT sy-subrc IS INITIAL.
*   No document exists for the specified data
    MESSAGE  e842(m7)        INTO  ls_return-message.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type      = 'E'
        cl        = 'M7'
        number    = '842'
*       par1      = sy-msgv1
*       par2      = sy-msgv2
*       par3      = sy-msgv3
*       par4      = sy-msgv4
*       LOG_NO    = ' '
*       LOG_MSG_NO = ' '
        parameter = ret_parameter
        row       = ret_row
        field     = ret_field
      IMPORTING
        return    = ls_return.
    APPEND ls_return         TO  return.
  ENDIF.

*----------------------------------------------------------------------*
* convert the data of the MM doc items into the external format
*----------------------------------------------------------------------*
  LOOP AT i_mseg.
    IF  lv_flag_check_plants_2 = abap_true  OR
        lv_flag_check_bwart_2  = abap_true.
*     we have to check the authority here
      PERFORM  check_authorities
                             TABLES    return
                             USING     lv_flag_check_plants_2 " check plants ?
                                       i_mseg-werks
                                       lv_flag_check_bwart_2  " check movement type ?
                                       i_mseg-bwart
                             CHANGING  lv_flag_auth_error.

      IF  lv_flag_auth_error = abap_true.
        DELETE               i_mseg.
        CONTINUE.            " take the next entry
      ENDIF.
    ENDIF.

*   convert the fields of the MM items into the external format
    CALL FUNCTION 'MP2E_MSEG_TO_2017_GM_ITEM_SHOW' "#EC CI_USAGE_OK[2438006]
      EXPORTING
        mseg                         = i_mseg
      CHANGING
        bapi2017_gm_item_show        = goodsmvt_items
      EXCEPTIONS
        error_converting_curr_amount = 1
        OTHERS                       = 2.

*   IF SY-SUBRC is initial.                                 "v_n1259654
    APPEND                 goodsmvt_items.
*   else.
*     CALL FUNCTION 'BALW_BAPIRETURN_GET2'
*        EXPORTING
*             TYPE       = SY-MSGTY
*             CL         = SY-MSGID
*             NUMBER     = SY-MSGNO
*             par1       = sy-msgv1
*             par2       = sy-msgv2
*             par3       = sy-msgv3
*             par4       = sy-msgv4
*             LOG_NO     = ' '
*             LOG_MSG_NO = ' '
*             PARAMETER  = RET_PARAMETER
*             ROW        = RET_ROW
*             FIELD      = RET_FIELD
*        IMPORTING
*             RETURN     = ls_RETURN.
*   APPEND ls_return         to  RETURN.
* ENDIF.                                                    "^_n1259654


*ENHANCEMENT-POINT BAPI_GOODSMVT_GETITEMS_03 SPOTS ES_SAPLMB_BUS2017.

*   create the key table for the MM doc headers when the key changes
    IF  NOT ( i_mseg-mblnr = ls_mkpf_key_old-mblnr  AND
              i_mseg-mjahr = ls_mkpf_key_old-mjahr ).
      MOVE : i_mseg-mblnr    TO  ls_mkpf_key_old-mblnr,
             i_mseg-mjahr    TO  ls_mkpf_key_old-mjahr,
             i_mseg-mblnr    TO  ls_mkpf_key-mblnr,
             i_mseg-mjahr    TO  ls_mkpf_key-mjahr.
      COLLECT  ls_mkpf_key   INTO  lt_mkpf_key.
    ENDIF.
  ENDLOOP.

  FREE                       i_mseg.

*----------------------------------------------------------------------*
* read and convert the data of the MM doc headers
*----------------------------------------------------------------------*
  IF  NOT lt_mkpf_key[] IS INITIAL.
*   key table is filled / read all fields of the MM doc headers
    SELECT *                   FROM  mkpf
      INTO TABLE i_mkpf
      FOR ALL ENTRIES IN lt_mkpf_key
         WHERE  mblnr =  lt_mkpf_key-mblnr
           AND  mjahr =  lt_mkpf_key-mjahr.

    IF sy-subrc IS INITIAL.
      SORT  i_mkpf           BY  mblnr mjahr.

*     convert the data of the MM doc headers into the external format
      LOOP AT i_mkpf.
        CALL FUNCTION 'MAP2E_MKPF_TO_B2017_GM_HEAD_02'
          EXPORTING
            mkpf                = i_mkpf
          CHANGING
            bapi2017_gm_head_02 = goodsmvt_header.
        APPEND goodsmvt_header.
      ENDLOOP.
    ENDIF.
  ENDIF.

*ENHANCEMENT-POINT BAPI_GOODSMVT_GETITEMS_G7 SPOTS ES_SAPLMB_BUS2017.
ENDFUNCTION.

*----------------------------------------------------------------------*
*    check_authorities
*----------------------------------------------------------------------*

FORM check_authorities
  TABLES    lt_return        TYPE  bapirettab
  USING     l_flag_werks     TYPE  abap_bool
            l_werks          TYPE  werks_d
            l_flag_bwart     TYPE  abap_bool
            l_bwart          TYPE  bwart
  CHANGING  l_flag_error     TYPE  abap_bool.

*----------------------------------------------------------------------*
* check whether the user has the required authorization for displaying
* MM docs for plant and movement type
*----------------------------------------------------------------------*

  DATA : ls_return           LIKE  LINE OF lt_return.

*----------------------------------------------------------------------*

  MOVE  abap_false           TO  l_flag_error.

* check authorization for the plant
  IF l_flag_werks = abap_true.
    IF  NOT cl_mmim_auth=>check( i_object = 'M_MSEG_WMB'
                                 i_value1 = l_werks ) IS INITIAL.
*     missing authorization for the plant
      MOVE  l_werks          TO  sy-msgv1.

*     You have no authorization for this transaction in plant &
      MESSAGE e120(m7)       WITH  l_werks
                             INTO  ls_return-message.

      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type      = 'E'
          cl        = 'M7'
          number    = '120'
          par1      = sy-msgv1
*         par2      = sy-msgv2
*         par3      = sy-msgv3
*         par4      = sy-msgv4
*         LOG_NO    = ' '
*         LOG_MSG_NO = ' '
          parameter = ret_parameter
          row       = ret_row
          field     = ret_field
        IMPORTING
          return    = ls_return.

      COLLECT ls_return      INTO  lt_return.
      MOVE  abap_true        TO  l_flag_error.
      RETURN.                " leave thie subroutine
    ENDIF.
  ENDIF.

* check authorization for the movement type
  IF  l_flag_bwart = abap_true.
    IF  NOT cl_mmim_auth=>check( i_object = 'M_MSEG_BMB'
                                 i_value1 = l_bwart ) IS INITIAL.
*     missing authorization for the movement type
      MOVE  l_bwart        TO  sy-msgv1.

*     You have no authorization for this transaction with movement type &
      MESSAGE e121(m7)       WITH  l_bwart
                             INTO  ls_return-message.

      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type      = 'E'
          cl        = 'M7'
          number    = '121'
          par1      = sy-msgv1
*         par2      = sy-msgv2
*         par3      = sy-msgv3
*         par4      = sy-msgv4
*         LOG_NO    = ' '
*         LOG_MSG_NO = ' '
          parameter = ret_parameter
          row       = ret_row
          field     = ret_field
        IMPORTING
          return    = ls_return.

      COLLECT ls_return      INTO  lt_return.
      MOVE  abap_true      TO  l_flag_error.
    ENDIF.
  ENDIF.

ENDFORM.                    " check_authorities.

*----------------------------------------------------------------------*
