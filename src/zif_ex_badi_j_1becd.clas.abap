class ZIF_EX_BADI_J_1BECD definition
  public
  final
  create public .

*"* public components of class ZIF_EX_BADI_J_1BECD
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_BADI_J_1BECD .
protected section.
*"* protected components of class ZIF_EX_BADI_J_1BECD
*"* do not include other source files here!!!
private section.
*"* private components of class ZIF_EX_BADI_J_1BECD
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZIF_EX_BADI_J_1BECD IMPLEMENTATION.


method IF_EX_BADI_J_1BECD~FILL_REGISTER_0000.
*   FIELD-SYMBOLS: <bukrs> TYPE ANY.
*   ASSIGN: ('(J_1BECD_MAIN)P_BUKRS') TO <bukrs>.
*
*   data: w_zsped003   TYPE zsped003.
*   SELECT SINGLE *
*     from ZSPED003
*     into w_zsped003
*   where bukrs = <bukrs>.
*
*  CHECK sy-subrc = 0.
*
*  CS_REG_0000-IDENT_MF = 'S'.

endmethod.


method IF_EX_BADI_J_1BECD~FILL_REGISTER_0020.
endmethod.


METHOD IF_EX_BADI_J_1BECD~FILL_REGISTER_I051.

*FIELD-SYMBOLS: <BUKRS> TYPE ANY.
*DATA: V_ERDAT TYPE SKB1-ERDAT.
*
*ASSIGN ('(J_1BECD_MAIN)P_BUKRS') TO <BUKRS>.
*
*IF IS_REG_I050-DT_ALT IS INITIAL.
*
*  SELECT SINGLE ERDAT
*    INTO V_ERDAT
*    FROM SKB1
*    WHERE BUKRS = <BUKRS>
*      AND SAKNR = IS_REG_I050-COD_CTA.
*
*  IF SY-SUBRC = 0.
*
*    MOVE V_ERDAT TO IS_REG_I050-DT_ALT.
*
*  ENDIF.
*
*ENDIF.

  FIELD-SYMBOLS: <BUKRS> TYPE ANY.
  FIELD-SYMBOLS: <GJAHR> TYPE ANY.
  FIELD-SYMBOLS: <VERSN> TYPE ANY.
  DATA: V_CONTA_REF TYPE ZSPED002-CONTA_REF,
        V_KOSAR     TYPE ZSPED002-KOSAR,
        V_KOSTL     TYPE CSKS-KOSTL,
        V_KOSARC    TYPE CSKS-KOSAR,
        AUX         TYPE J_1BECD_I051_4_S,
        DAY(2)      TYPE C,
        MONTH(2)    TYPE C,
        YEAR(4)     TYPE C,
        DTA(8)      TYPE C.

  IF SY-CPROG = 'Z_SPED_CONTABIL'.
    ASSIGN ('(Z_SPED_CONTABIL)P_BUKRS') TO <BUKRS>.
    ASSIGN ('(Z_SPED_CONTABIL)P_GJAHR') TO <GJAHR>.
    ASSIGN ('(Z_SPED_CONTABIL)P_FSTAT') TO <VERSN>.
  ELSE.
    ASSIGN ('(J_1BECD_MAIN)P_BUKRS') TO <BUKRS>.
    ASSIGN ('(J_1BECD_MAIN)P_GJAHR') TO <GJAHR>.
    ASSIGN ('(J_1BECD_MAIN)P_FSTAT') TO <VERSN>.
  ENDIF.

  DAY   =  '31' .
  MONTH =  '12' .
  YEAR  =  <GJAHR> .

  CONCATENATE YEAR MONTH DAY INTO DTA.


  SELECT CONTA_REF KOSAR
    INTO (V_CONTA_REF, V_KOSAR)
    FROM ZSPED002
    WHERE BUKRS = <BUKRS>
    and   VERSN = <VERSN>
    AND SAKNR = IS_REG_I050-COD_CTA.

    " Tipo de Centro de custo diferente de nulo.
    IF V_KOSAR IS NOT INITIAL.

      SELECT KOSTL KOSAR
      INTO (V_KOSTL, V_KOSARC)
      FROM CSKS
      WHERE KOKRS IN ('MAGI','MGLG')
        AND BUKRS = <BUKRS>
        AND KOSAR = V_KOSAR
        AND DATAB <= DTA
        AND DATBI > SY-DATUM .


        " |I051|10| CSKS-KOSTL | ZSPED002-CONTA_REF|
        AUX-REG = 'I051'.
        AUX-COD_ENT_REF = '1'.
        IF '0022_0023' CS <BUKRS> .
          AUX-COD_ENT_REF = '2'.
        ENDIF.
        AUX-COD_CCUS = V_KOSTL.
        AUX-COD_CTA_REF = V_CONTA_REF.

        APPEND AUX TO ET_REG_I051.

      ENDSELECT.

    ELSE.

      " Branco
      "|I051|10|| ZSPED002-CONTA_REF|
      AUX-REG = 'I051'.
      AUX-COD_ENT_REF = '1'.
      IF '0022_0023' CS <BUKRS> .
        AUX-COD_ENT_REF = '2'.
      ENDIF.
      AUX-COD_CCUS = ''.
      AUX-COD_CTA_REF = V_CONTA_REF.

      APPEND AUX TO ET_REG_I051.

    ENDIF.

  ENDSELECT.

ENDMETHOD.


METHOD if_ex_badi_j_1becd~fill_register_i155.

  DATA vl_dats TYPE dats.

  TYPES:  BEGIN OF ty_bkpf,
       bukrs TYPE bkpf-bukrs,
       belnr TYPE bkpf-belnr,
       gjahr TYPE bkpf-gjahr,
       budat TYPE bkpf-budat,
     END OF ty_bkpf.

  TYPES: BEGIN OF ty_bseg,
         bukrs TYPE bseg-bukrs,
         belnr TYPE bseg-belnr,
         gjahr TYPE bseg-gjahr,
         buzei TYPE bseg-buzei,
         shkzg TYPE bseg-shkzg,
         dmbtr TYPE bseg-dmbtr,
         hkont TYPE bseg-hkont,
       END OF ty_bseg.

  DATA: t_bkpf TYPE STANDARD TABLE OF ty_bkpf,
        t_bseg TYPE STANDARD TABLE OF ty_bseg,
        w_bseg TYPE ty_bseg,
        ls_i155 TYPE j_1becd_i155_4_s.

  FIELD-SYMBOLS: <bukrs> TYPE ANY,
                 <gjahr> TYPE ANY.

  DATA: w_saknr TYPE zcontas_sped-saknr.

  ASSIGN: ('(J_1BECD_MAIN)P_BUKRS') TO <bukrs>,
          ('(J_1BECD_MAIN)P_GJAHR') TO <gjahr>.

  CHECK iv_posting_period EQ '010'.

  READ TABLE it_reg_i155 INTO ls_i155 INDEX 1.

  SELECT SINGLE saknr
    FROM zcontas_sped INTO w_saknr
    WHERE bukrs EQ <bukrs> AND
          gjahr EQ <gjahr> AND
          saknr EQ ls_i155-cod_cta.

** Se encontrar a conta, fazer o calculo
  IF sy-subrc = 0.

    READ TABLE it_reg_i155 INTO ls_i155 INDEX 1.
    IF sy-subrc <> 0.
      CLEAR ls_i155.
      ls_i155-cod_cta = is_skc1a-saknr.
      ls_i155-ind_dc_ini = 'C'.
      ls_i155-ind_dc_fin = 'C'.
    ELSE.
      CLEAR: ls_i155-vl_cred, ls_i155-vl_deb.
    ENDIF.

    vl_dats = is_reg_i150-dt_fin.
    vl_dats = '20101028'.

    SELECT bukrs belnr gjahr budat FROM bkpf
    INTO TABLE t_bkpf
    WHERE bukrs = is_skc1a-bukrs AND
          gjahr = is_skc1a-gjahr AND
          budat BETWEEN is_reg_i150-dt_ini AND vl_dats AND
          bstat = space.

    CHECK sy-subrc IS INITIAL.
    DATA ETL69C4R6532 TYPE TABLE OF BSEG.
DATA LT_FIELDS_L69C4R4420 TYPE FAGL_T_FIELD.
LT_FIELDS_L69C4R4420 = VALUE #( ( LINE = 'BUKRS' )
 ( LINE = 'BELNR' )
 ( LINE = 'GJAHR' )
 ( LINE = 'BUZEI' )
 ( LINE = 'SHKZG' )
 ( LINE = 'DMBTR' )
 ( LINE = 'HKONT' )
 ).

CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = T_BKPF
              I_WHERE_CLAUSE = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR AND HKONT = { CL_ABAP_DYN_PRG=>QUOTE( IS_SKC1A-SAKNR ) }|
              IT_FIELDLIST = LT_FIELDS_L69C4R4420
    IMPORTING ET_BSEG = ETL69C4R6532
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC = 0 AND LINES( ETL69C4R6532 ) > 0.
  MOVE-CORRESPONDING ETL69C4R6532 TO T_BSEG.
  SY-DBCNT = LINES( ETL69C4R6532 ).
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.


    DELETE t_bseg WHERE dmbtr IS INITIAL.

    LOOP AT t_bseg INTO w_bseg.
      IF w_bseg-shkzg = 'H'.
        ls_i155-vl_cred = ls_i155-vl_cred + w_bseg-dmbtr.
      ELSEIF w_bseg-shkzg = 'S'.
        ls_i155-vl_deb = ls_i155-vl_deb + w_bseg-dmbtr.
      ENDIF.
    ENDLOOP.

    IF NOT ls_i155-vl_sld_ini IS INITIAL OR
       NOT ls_i155-vl_sld_fin IS INITIAL OR
       NOT ls_i155-vl_deb     IS INITIAL OR
       NOT ls_i155-vl_cred    IS INITIAL.
      APPEND ls_i155 TO et_reg_i155.
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD if_ex_badi_j_1becd~fill_register_i250.

*** Para conta abaixo, não gerar registro i250 - 14.06.2010
*  IF is_reg_i250-cod_cta  = '0000212200'.
*
*    FIELD-SYMBOLS: <i250>  TYPE j_1becd_i250_4_t.
*    ASSIGN: ('(J_1BECD_MAIN)gt_i250') TO <i250>.
*
*    IF sy-subrc = 0.
*      CLEAR <i250>.
*    ENDIF.
*  ENDIF.
*
*  DATA: v1_vbund         TYPE bseg-vbund,
*        v2_setname       TYPE setleaf-setname,
*        v3_lineid        TYPE setleaf-lineid,
*        v4_setclass      TYPE setleaf-setclass,
*        v5_descript      TYPE setlinet-descript.
*
*
*  CLEAR: v1_vbund, v2_setname, v3_lineid,  v4_setclass, v5_descript  .
*
*  SELECT vbund
*    FROM bseg
*    INTO v1_vbund
*   WHERE  bukrs EQ iv_bukrs
*     AND belnr EQ iv_belnr
*     AND gjahr EQ iv_gjahr.
*
*    IF v1_vbund IS NOT INITIAL.
*
*      SELECT SINGLE setname lineid setclass
*        FROM setleaf
*        INTO (v2_setname, v3_lineid, v4_setclass)
*       WHERE setname EQ 'ECD_BLOCO_I250'
*         AND valfrom EQ v1_vbund.
*
*
*      SELECT SINGLE descript
*       FROM setlinet
*       INTO v5_descript
*      WHERE setclass EQ v4_setclass
*        AND setname  EQ v2_setname
*        AND lineid   EQ v3_lineid.
*
*
*      IF sy-subrc IS INITIAL.
*        ev_cod_part = v5_descript.
*      ENDIF.
*
*      check 1 = 2.
*
*    ENDIF.
*
*  ENDSELECT.
ENDMETHOD.


  method IF_EX_BADI_J_1BECD~FILL_COMPANY_NAME.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_0035.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_0150.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_0180.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_I012.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_I030.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_I052.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_I053.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_I100.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_I151.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_I157.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_I310.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_I355.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_I500.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_I510.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_I550.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_J100.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_J200.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_J210.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_J215.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_J930.
  endmethod.


  method IF_EX_BADI_J_1BECD~FILL_REGISTER_J935.
  endmethod.
ENDCLASS.
