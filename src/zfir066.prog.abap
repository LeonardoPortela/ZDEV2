*&---------------------------------------------------------------------*
*& Report  ZFIR066
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfir066.

TABLES: bsis, zhrst_efd_e1250m, sscrfields.

TYPE-POOLS: icon.

TYPES: BEGIN OF ty_bsis,
         bukrs TYPE bsis-bukrs,
         gjahr TYPE bsis-gjahr,
         belnr TYPE bsis-belnr,
         xblnr TYPE bsis-xblnr,
         gsber TYPE bsis-gsber,
         bldat TYPE bsis-bldat,
         hkont TYPE bsis-hkont,
         budat TYPE bsis-budat,
         blart TYPE bsis-blart,
       END OF ty_bsis.

TYPES: BEGIN OF ty_with_item,
         wt_withcd TYPE with_item-wt_withcd,
         wt_qsshh  TYPE with_item-wt_qsshh,
         wt_acco   TYPE with_item-wt_acco,
         bukrs     TYPE with_item-bukrs,
         belnr     TYPE with_item-belnr,
         gjahr     TYPE with_item-gjahr,
         witht     TYPE with_item-witht,
         wt_qbshh  TYPE with_item-wt_qbshh,
       END OF ty_with_item.

TYPES: BEGIN OF ty_lfa1,
         stcd1 TYPE lfa1-stcd1,
         stcd2 TYPE lfa1-stcd2,
         stkzn TYPE lfa1-stkzn,
         name1 TYPE lfa1-name1,
         lifnr TYPE lfa1-lifnr,
       END OF ty_lfa1.

DATA: it_bsis             TYPE TABLE OF ty_bsis,
      it_with_item        TYPE TABLE OF ty_with_item,
      it_with_item_aux    TYPE TABLE OF ty_with_item,
      it_lfa1             TYPE TABLE OF ty_lfa1,
      it_zhrst_efd_e1250m TYPE TABLE OF zhrst_efd_e1250m,
      it_bkpf             TYPE TABLE OF bkpf,

      wa_bsis             TYPE  ty_bsis,
      wa_with_item        TYPE  ty_with_item,
      wa_lfa1             TYPE  ty_lfa1,
      wa_zhrst_efd_e1250m TYPE  zhrst_efd_e1250m,
      wa_bkpf             TYPE  bkpf.

DATA: ano(4) TYPE c.

CONSTANTS: v_wt_qsshh01 TYPE p LENGTH 15 DECIMALS 3  VALUE '0.012',
           v_wt_qsshh02 TYPE p LENGTH 15 DECIMALS 3  VALUE '0.001',
           v_wt_qsshh03 TYPE p LENGTH 15 DECIMALS 3  VALUE '0.002'.


SELECTION-SCREEN: FUNCTION KEY 1.

SELECTION-SCREEN  BEGIN OF BLOCK b1.
  SELECT-OPTIONS: p_bukrs FOR bsis-bukrs OBLIGATORY,
                  p_data FOR bsis-budat  OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.


INITIALIZATION.

  PERFORM cria_botao.

AT SELECTION-SCREEN.
  IF sy-ucomm = 'FC01'.
    CALL TRANSACTION 'ZFIS48' AND SKIP FIRST SCREEN.
  ENDIF.

START-OF-SELECTION.

  PERFORM seleciona_dados.
  PERFORM trata_dados.

END-OF-SELECTION.


FORM cria_botao.
  DATA: wa_button TYPE smp_dyntxt.

  wa_button-text  = 'Gera Rel.Funrural'.
  wa_button-icon_id = icon_biw_report.
  sscrfields-functxt_01 = wa_button.
ENDFORM.


FORM seleciona_dados.
  CLEAR ano.
  ano = p_data-low+0(4).

  SELECT  bukrs
          gjahr
          belnr
          xblnr
          gsber
          bldat
          hkont
          budat
          blart
     FROM bsis INTO TABLE it_bsis
    WHERE bukrs IN p_bukrs
    AND   hkont IN ('0000213207','0000213213')
    AND   gjahr EQ ano
    AND   budat IN p_data
    AND   blart NOT IN ('ZG', 'SI').

  IF it_bsis IS NOT INITIAL.

    SELECT * FROM bkpf INTO TABLE it_bkpf
      FOR ALL ENTRIES IN it_bsis
    WHERE bukrs EQ it_bsis-bukrs
      AND belnr EQ it_bsis-belnr
      AND gjahr EQ it_bsis-gjahr.


    SELECT  wt_withcd
            wt_qsshh
            wt_acco
            bukrs
            belnr
            gjahr
            witht
            wt_qbshh
      FROM with_item INTO TABLE it_with_item_aux
      FOR ALL ENTRIES IN it_bsis
      WHERE bukrs EQ it_bsis-bukrs
      AND   belnr EQ it_bsis-belnr
      AND   gjahr EQ it_bsis-gjahr
      AND   witht IN ('IF', 'FR', 'SE', 'SF').

    DELETE  it_with_item_aux WHERE ( witht = 'FR' OR witht = 'IF' )  AND
                                     wt_qbshh = 0.

    SORT it_with_item_aux  BY bukrs belnr gjahr witht. "ALRS
    DELETE it_with_item_aux WHERE wt_qbshh = 0.

    MOVE it_with_item_aux TO it_with_item.

    IF it_with_item IS NOT INITIAL.
      SELECT  stcd1
              stcd2
              stkzn
              name1
              lifnr
        FROM lfa1 INTO TABLE it_lfa1
        FOR ALL ENTRIES IN it_with_item
       WHERE lifnr EQ it_with_item-wt_acco
        AND  stkzn EQ 'X'.
    ENDIF.
  ELSE.
    MESSAGE 'Nenhum dado encontrado p/ o processamento.' TYPE 'I'.
  ENDIF.
ENDFORM.

FORM trata_dados.
  DATA: vbelnr(10) TYPE c,
        vgjhar(4)  TYPE c.


  LOOP AT it_bsis INTO wa_bsis.


    READ TABLE it_bkpf INTO wa_bkpf WITH KEY  bukrs = wa_bsis-bukrs
                                              gjahr = wa_bsis-gjahr
                                              belnr = wa_bsis-belnr.
    IF sy-subrc = 0.

      CLEAR: vbelnr, vgjhar.

      vbelnr = wa_bkpf-awkey+0(10).
      vgjhar = wa_bkpf-awkey+10(4).


      SELECT SINGLE belnr, stblg FROM rbkp INTO @DATA(wa_rbkp)
        WHERE belnr EQ @vbelnr
        AND   gjahr EQ @vgjhar.


      SELECT SINGLE ebeln, ebelp FROM ekbe INTO @DATA(wa_ekbe)
        WHERE belnr EQ @vbelnr
        AND   gjahr EQ @vgjhar.

      SELECT SINGLE bukrs, bsart  FROM ekko INTO @DATA(wa_ekko)
       WHERE ebeln EQ @wa_ekbe-ebeln
        AND  bukrs EQ @wa_bkpf-bukrs.

      SELECT SINGLE ebeln, matkl  FROM ekpo INTO @DATA(wa_ekpo)
       WHERE ebeln EQ @wa_ekbe-ebeln
        AND  bukrs EQ @wa_bkpf-bukrs.

    ENDIF.

    IF wa_rbkp-stblg IS INITIAL.

      READ TABLE it_with_item  INTO wa_with_item WITH KEY belnr  = wa_bsis-belnr
                                                          bukrs = wa_bsis-bukrs
                                                          gjahr = wa_bsis-gjahr.
      IF sy-subrc = 0.
        wa_zhrst_efd_e1250m-dmbtr     =    wa_with_item-wt_qsshh * -1.

        IF ( ( wa_with_item-witht = 'SE' ) OR ( wa_with_item-witht = 'SF' ) ) OR  ( wa_ekko-bsart = 'ZSEM' ) OR ( wa_ekpo-matkl = '750110' ). "CS2021000436
***        IF ( ( wa_with_item-witht = 'SE' ) OR ( wa_with_item-witht = 'SF' ) ) OR  ( wa_ekko-bsart = 'ZSEM' ).

          wa_zhrst_efd_e1250m-vrcp       = 0.
          wa_zhrst_efd_e1250m-vrrat      = 0.

          IF ( wa_ekko-bsart = 'ZSEM' ) OR ( wa_ekpo-matkl = '750110' ). "CS2021000436
***          IF ( wa_ekko-bsart = 'ZSEM' ).
            wa_zhrst_efd_e1250m-ind_aquis =  4.
          ENDIF.

        ELSE.

          wa_zhrst_efd_e1250m-ind_aquis =  1.
          wa_zhrst_efd_e1250m-vrcp      =  ( wa_with_item-wt_qsshh * -1 ) * v_wt_qsshh01.
          wa_zhrst_efd_e1250m-vrrat     =  ( wa_with_item-wt_qsshh * -1 ) * v_wt_qsshh02.

        ENDIF.



        wa_zhrst_efd_e1250m-vrsenar    =  ( wa_with_item-wt_qsshh * -1 ) * v_wt_qsshh03.
***        wa_zhrst_efd_e1250m-ind_acquis = '1'.
        wa_zhrst_efd_e1250m-ind_acquis = ' '. "CS2021000436



        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr  = wa_with_item-wt_acco
                                                 stkzn  = 'X'.
        IF sy-subrc = 0.
          wa_zhrst_efd_e1250m-cpf    = wa_lfa1-stcd2.
          wa_zhrst_efd_e1250m-cname  = wa_lfa1-name1.
          wa_zhrst_efd_e1250m-bukrs  = wa_bsis-bukrs.

          IF wa_bsis-gsber = '9121'.
            wa_zhrst_efd_e1250m-branch    =   '0121'.
          ELSE.
            wa_zhrst_efd_e1250m-branch    =   wa_bsis-gsber.
          ENDIF.

          wa_zhrst_efd_e1250m-belnr     =   wa_bsis-belnr.
          wa_zhrst_efd_e1250m-gjahr     =   wa_bsis-gjahr.
          wa_zhrst_efd_e1250m-budat     =   wa_bsis-budat.
          wa_zhrst_efd_e1250m-xblnr     =   wa_bsis-xblnr.
          wa_zhrst_efd_e1250m-bldat     =   wa_bsis-bldat.

          APPEND wa_zhrst_efd_e1250m TO it_zhrst_efd_e1250m.
        ENDIF.
      ENDIF.

    ENDIF.

    CLEAR: wa_bsis, wa_with_item, wa_zhrst_efd_e1250m.
  ENDLOOP.


  IF it_zhrst_efd_e1250m IS NOT INITIAL.
    MODIFY zhrst_efd_e1250m FROM TABLE it_zhrst_efd_e1250m.
    MESSAGE 'Processo Gerado com Sucesso!' TYPE 'I'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'FUNRURAL'.
      CALL TRANSACTION 'ZFIS48' AND SKIP FIRST SCREEN.
  ENDCASE.
ENDMODULE.
