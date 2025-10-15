FUNCTION zexit_sapmf02k_001.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_BUT000) LIKE  BUT000 STRUCTURE  BUT000 OPTIONAL
*"     VALUE(I_LFA1) LIKE  LFA1 STRUCTURE  LFA1
*"     VALUE(I_LFB1) LIKE  LFB1 STRUCTURE  LFB1
*"     VALUE(I_LFM1) LIKE  LFM1 STRUCTURE  LFM1
*"     VALUE(I_ADDRHANDLE) LIKE  ADDR1_SEL-ADDRHANDLE OPTIONAL
*"  TABLES
*"      T_LFBK STRUCTURE  LFBK OPTIONAL
*"      T_LFB5 STRUCTURE  LFB5 OPTIONAL
*"      T_LFZA STRUCTURE  LFZA OPTIONAL
*"      T_LFBW STRUCTURE  LFBW OPTIONAL
*"      T_LFAS STRUCTURE  LFAS OPTIONAL
*"      T_LFAT STRUCTURE  LFAT OPTIONAL
*"      T_LFLR STRUCTURE  LFLR OPTIONAL
*"      T_LFM2 STRUCTURE  LFM2 OPTIONAL
*"      T_WYT1 STRUCTURE  WYT1 OPTIONAL
*"      T_WYT1T STRUCTURE  WYT1T OPTIONAL
*"      T_WYT3 STRUCTURE  WYT3 OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"      ZCX_ERROR
*"      ZCX_INTEGRACAO
*"----------------------------------------------------------------------

  FREE: lc_error.

  CONSTANTS: lc_excessao TYPE cabn-atnam VALUE 'ZADCN_EXCECAO',
             lc_consulta TYPE cabn-atnam VALUE 'ZADCN_CONSULTA_FONTES'.


  TYPES: BEGIN OF tp_bild,
           mindx    LIKE sy-tabix,
           windx    LIKE sy-tabix,
           sindx    LIKE sy-tabix,
           atwrt    LIKE cawn-atwrt,
           mmaus(1) TYPE c,
           mwaus(1) TYPE c,
         END OF tp_bild,

         BEGIN OF char_description,
           langu LIKE sy-langu,
           atbez LIKE cabnt-atbez,
           atue1 LIKE cabnt-atue1,
           atue2 LIKE cabnt-atue2,
           atkle LIKE cabn-atkle,
         END   OF char_description,

         BEGIN OF tp_mi,
           atinn                    LIKE cabn-atinn,
           mname                    LIKE cabnt-atbez,
           mindx                    LIKE sy-index,
           msele                    TYPE c,
           mmark                    TYPE c,
           merbe                    LIKE rctms-merbe,
           lwmng                    TYPE c,
           mmerb                    LIKE rctms-merbe,
           moerb                    LIKE rctms-merbe,
           stamm                    TYPE c,
           omadd                    TYPE c,
           group                    LIKE cusdt-grtxt,
           tabst                    LIKE cusd-tabst,
           gcomp                    TYPE c,
           clint                    LIKE cabn-clint,
           loevm                    TYPE x,
           maws0                    TYPE c,
           maws1                    TYPE c,
           maws2                    TYPE c,
           maws3                    TYPE c,
           maws4                    TYPE c,
           maws5                    TYPE c,
           maws6                    TYPE c,
           maws7                    TYPE c,
           maws8                    TYPE c,
           maws9                    TYPE c,
           dinkb                    LIKE rmclm-dinkbu,
           atue1                    LIKE cabnt-atue1,
           atue2                    LIKE cabnt-atue2,
           atcod                    LIKE cawn-atcod,
           mrkin                    LIKE sy-tabix,
           wertm                    TYPE c,
           hiera                    TYPE c,
           atnam                    LIKE cabn-atnam,
           wivon                    LIKE sy-tabix,
           wibis                    LIKE sy-tabix,
           ocawn                    TYPE x,
           vclin                    LIKE klah-clint,
           aterf                    LIKE cabn-aterf,
           oaterf                   LIKE cabn-aterf,
           atein                    LIKE cabn-atein,
           oatein                   LIKE cabn-atein,
           atgla                    LIKE cabn-atgla,
           atwrd                    LIKE cabn-atwrd,
           atfod                    LIKE cabn-atfod,
           miwrt                    LIKE cabn-atint,
           oatint                   LIKE cabn-atint,
           mswrt                    LIKE cabn-atson,
           oatson                   LIKE cabn-atson,
           atinp                    LIKE cabn-atinp,
           atvie                    LIKE cabn-atvie,
           atglo                    LIKE cabn-atglo,
           atfor                    LIKE cabn-atfor,
           dtype                    LIKE cabn-atfor,
           mazst                    LIKE cabn-anzst,
           mazdz                    LIKE cabn-anzdz,
           mvorz                    LIKE cabn-atvor,
           mscha                    LIKE cabn-atsch,
           atvsc                    LIKE cabn-atvsc,
           meinh                    LIKE cabn-msehi,
           mseh6                    LIKE t006a-mseh6,
           atkle                    LIKE cabn-atkle,
           oatkle                   LIKE cabn-atkle,
           atkon                    LIKE cabn-atkon,
           atdex                    LIKE cabn-atdex,
           atdim                    LIKE cabn-atdim,
           attab                    LIKE cabn-attab,
           atfel                    LIKE cabn-atfel,
           oattab                   LIKE cabn-attab,
           oatfel                   LIKE cabn-atfel,
           cudef                    LIKE cabn-clint,
           adtkz(1)                 TYPE c,
           athka                    LIKE cabn-athka,
           athko                    LIKE cabn-athko,
           atprt                    LIKE cabn-atprt,
           atprf                    LIKE cabn-atprf,
           werks                    LIKE cabn-werks,
           kaart                    LIKE cabn-katalogart,
           awmng                    LIKE cabn-auswahlmge,
           kztxt                    TYPE x,
           cl                       TYPE char_description OCCURS 5,
           wi_is_language_dependent TYPE c,
         END   OF tp_mi.

**/ Internal Tables
  DATA: lt_bild TYPE TABLE OF tp_bild,
        lt_mi   TYPE TABLE OF tp_mi.

**/ Work areas
  DATA: ls_bild    LIKE LINE OF lt_bild,
        ls_mi      LIKE LINE OF lt_mi,
        lwa_lfa1   TYPE lfa1,
        lc_validar TYPE char01.

**/ Variables
  DATA: lv_date TYPE atwrt,
        lv_cons TYPE atwrt.

  DATA: lv_xk02_force_cadastro  TYPE c.
  DATA: lv_xk02_termo_al5bank   TYPE c.

  IF sy-tcode = 'BP' OR sy-batch = abap_on OR sy-tcode  = 'ZWS0004'. "*-#149089-07.01.2025-JT-inicio
*  Não deixar duplicar CPF

*-#155327-16.10.2024-JT-inicio
    PERFORM f_check_validar_dados_fornece  USING i_but000
                                                 i_lfa1
                                                 i_lfm1  "*-#156755-31.10.2024-JT-inicio
                                                 i_lfb1
                                        CHANGING lc_validar.
    CHECK lc_validar = abap_true.
*-#155327-16.10.2024-JT-fim

    CLEAR: lv_xk02_force_cadastro.                          "US 77981
    IMPORT lv_xk02_force_cadastro TO lv_xk02_force_cadastro FROM MEMORY ID 'XK02_FORCE_CADASTRO'. "US 77981
    DELETE FROM MEMORY ID 'XK02_FORCE_CADASTRO'.


    DATA: v_ktokk LIKE setleaf-valfrom.

    SELECT SINGLE valfrom INTO v_ktokk
     FROM setleaf
    WHERE setname EQ 'Z_VERIF_CPF'
      AND valfrom EQ i_lfa1-ktokk.

    IF ( sy-subrc EQ 0 ) AND ( lv_xk02_force_cadastro EQ abap_false ).
      PERFORM f_consistir_cpf USING i_but000
                                    i_lfa1-lifnr
                                    i_lfa1-stcd2
                                    i_lfa1-ktokk
                                    i_lfa1-sperr
                                    i_lfa1-loevm
                                    i_lfa1-nodel.
    ENDIF.

    "Não deixa duplicar CUIT (Argentina)
    IF ( i_lfa1-ktokk(1) = 'Y' ) AND ( lv_xk02_force_cadastro EQ abap_false ).
      PERFORM f_consistir_cuit USING i_but000
                                     i_lfa1-lifnr
                                     i_lfa1-stcd1.
    ENDIF.

*  Não deixar duplicar CNPJ
    IF i_lfa1-ktokk = 'ZFNJ' OR i_lfa1-ktokk = 'ZPRJ' OR i_lfa1-ktokk = 'ZFFJ' .

      IF ( sy-subrc NE 0 ) AND ( lv_xk02_force_cadastro EQ abap_false ).
        PERFORM f_consistir_cnpj USING i_but000
                                       i_lfa1-lifnr
                                       i_lfa1-stcd1
                                       i_lfa1-stcd3
                                       i_lfa1-regio
                                       i_lfa1-ktokk. "*-CS2024000622-19.09.2024-JT-#152691-fim
      ENDIF.
    ENDIF.

*  Não deixar duplicar CPF + Inscrição estadual juntos
    IF ( i_lfa1-ktokk = 'ZPRF' ) AND ( lv_xk02_force_cadastro EQ abap_false ).
      PERFORM f_consistir_cpf_insc USING i_but000
                                         i_lfa1-lifnr
                                         i_lfa1-stcd2
                                         i_lfa1-stcd3
                                         i_lfa1-regio.  "*-CS2024000622-26.07.2024-JT-#146685
    ENDIF.

*  Não deixar duplicar CNPJ + Inscrição estadual juntos
    IF ( i_lfa1-ktokk = 'ZFIC' ) "OR i_lfa1-ktokk = 'ZFNJ' )
   AND ( lv_xk02_force_cadastro EQ abap_false ).
      PERFORM f_consistir_cnpj_insc USING i_but000
                                          i_lfa1-lifnr
                                          i_lfa1-stcd1
                                          i_lfa1-stcd3
                                          abap_true.
    ENDIF.

*-CS2021001147 - 02.03.2022 - JT - inicio
    PERFORM f_validar_iban TABLES t_lfbk
                            USING i_lfa1-ktokk.
*-CS2021001147 - 02.03.2022 - JT - fim

*-CS2022000535-03.02.2023-#78407-JT-inicio
    PERFORM f_valida_conta  USING i_lfb1-bukrs
                                  i_lfa1-lifnr
                                  i_lfa1-stcd1
                                  i_lfa1-stcd2
                                  i_lfb1-akont.

  ENDIF.

  "183535 - BUG SOLTO - bloqueio expansão entre empresas
  IF sy-tcode EQ 'BP'.
    PERFORM f_grupo_fornec  USING  i_lfb1-bukrs
                                   i_lfa1-ktokk.
  ENDIF.


ENDFUNCTION.
