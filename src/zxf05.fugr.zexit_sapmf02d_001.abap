FUNCTION zexit_sapmf02d_001.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_BUT000) LIKE  BUT000 STRUCTURE  BUT000 OPTIONAL
*"     VALUE(I_KNA1) LIKE  KNA1 STRUCTURE  KNA1
*"     VALUE(I_KNB1) LIKE  KNB1 STRUCTURE  KNB1 OPTIONAL
*"     VALUE(I_KNVV) LIKE  KNVV STRUCTURE  KNVV OPTIONAL
*"     VALUE(I_ADDRHANDLE) LIKE  ADDR1_SEL-ADDRHANDLE OPTIONAL
*"  TABLES
*"      T_KNAS STRUCTURE  KNAS OPTIONAL
*"      T_KNAT STRUCTURE  KNAT OPTIONAL
*"      T_KNB5 STRUCTURE  KNB5 OPTIONAL
*"      T_KNBK STRUCTURE  KNBK OPTIONAL
*"      T_KNBW STRUCTURE  KNBW OPTIONAL
*"      T_KNVA STRUCTURE  KNVA OPTIONAL
*"      T_KNVD STRUCTURE  KNVD OPTIONAL
*"      T_KNVI STRUCTURE  KNVI OPTIONAL
*"      T_KNVK STRUCTURE  KNVK OPTIONAL
*"      T_KNVL STRUCTURE  KNVL OPTIONAL
*"      T_KNVP STRUCTURE  KNVP OPTIONAL
*"      T_KNZA STRUCTURE  KNZA OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"      ZCX_ERROR
*"      ZCX_INTEGRACAO
*"----------------------------------------------------------------------


**/ Constants
  CONSTANTS: lc_excessao TYPE cabn-atnam VALUE 'ZADCN_EXCECAO',
             lc_consulta TYPE cabn-atnam VALUE 'ZADCN_CONSULTA_FONTES'.

**/ Types
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
  DATA: ls_bild  LIKE LINE OF lt_bild,
        ls_mi    LIKE LINE OF lt_mi,
        ls_knvk  LIKE LINE OF t_knvk,
        lwa_kna1 TYPE kna1.

**/ Variables
  DATA: lv_date    TYPE atwrt,
        lv_cons    TYPE atwrt,
        lc_validar TYPE char01.

  DATA: vl_flg   TYPE c,
        vl_kunnr LIKE kna1-kunnr.

  IF sy-tcode = 'BP' OR sy-batch = abap_on OR sy-tcode  = 'ZWS0004'. "*-#149089-07.01.2025-JT-inicio

    vl_flg = 'X'.

*-#155327-16.10.2024-JT-inicio
    PERFORM f_check_validar_dados_cliente  USING i_but000
                                                 i_kna1
                                                 i_knb1
                                                 i_knvv
                                        CHANGING lc_validar.
    CHECK lc_validar = abap_true.
*-#155327-16.10.2024-JT-fim

*-- valida dados bancarios ----------------------
    PERFORM f_dados_bancarios TABLES t_knbk
                               USING i_kna1. "*-CS2024000622-18.09.2024-JT-#152328-inicio

*  Não deixar duplicar CPF
    IF i_kna1-ktokd = 'ZCNF' OR i_kna1-ktokd = 'ZCFU' OR
       i_kna1-ktokd = 'ZCFF'.
      PERFORM f_consistir_cpf_cli USING i_but000
                                        i_kna1-kunnr
                                        i_kna1-stcd2
                               CHANGING vl_flg vl_kunnr.
    ENDIF.

*  Não deixa duplicar CUIT (Argentina)
    IF i_kna1-ktokd(1) = 'Y'.
      PERFORM f_consistir_cuit_cli USING i_but000
                                         i_kna1-kunnr
                                         i_kna1-stcd1
                                CHANGING vl_flg vl_kunnr.
    ENDIF.

*  Não deixar duplicar CNPJ
    IF i_kna1-ktokd = 'ZCFJ'.
      PERFORM f_consistir_cnpj_cli USING i_but000
                                         i_kna1-kunnr
                                         i_kna1-stcd1
                                         i_kna1-stcd3
                                         i_kna1-regio
                                         i_kna1-ktokd
                                CHANGING vl_flg vl_kunnr.
    ENDIF.

*  Não deixar duplicar CPF + Inscrição estadual juntos
    IF i_kna1-ktokd = 'ZCPF'.
      PERFORM f_consistir_cpf_insc_cli USING i_but000
                                             i_kna1-kunnr
                                             i_kna1-stcd2
                                             i_kna1-stcd3
                                             i_kna1-regio
                                    CHANGING vl_flg vl_kunnr.
    ENDIF.

*  Não deixar duplicar CNPJ + Inscrição estadual juntos
    IF ( i_kna1-ktokd = 'ZCIC' ) OR ( i_kna1-ktokd = 'ZCNJ' ) OR ( i_kna1-ktokd = 'ZCPJ' ).
      PERFORM f_consistir_cnpj_insc_cli USING i_but000
                                              i_kna1-kunnr
                                              i_kna1-stcd1
                                              i_kna1-stcd3
                                              i_kna1-ktokd
                                     CHANGING vl_flg vl_kunnr.
    ENDIF.

*==> Begin of Estevão Borges de Lara - Audicon - 08.03.2019
*    FIELD-SYMBOLS: <fs_addr1_data> TYPE addr1_data,
*                   <fs_kna1>       TYPE kna1,
*                   <fs_ktokd>      TYPE rf02d-ktokd,
*                   <cliente>       TYPE rf02d-kunnr,
*                   <ref_cliente>   TYPE rf02d-ref_kunnr,
*                   <fs_bild>       LIKE lt_bild,
*                   <fs_mi>         LIKE lt_mi.

  ENDIF.

  IF sy-tcode EQ 'BP'.
    "183535 - bug solto - Cadastro BP - Cliente - bloqueio expansão entre empresas
    PERFORM f_grupo_cliente  USING  i_knb1-bukrs
                                    i_kna1-ktokd.
  ENDIF.


ENDFUNCTION.
