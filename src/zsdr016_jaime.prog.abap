*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 01/01/2012                                              &*
*& Descrição: Simulador de Vendas                                     &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP            DEVK920021   10.01.2012                            &*
*& Marcos Faneli   DEVK937401   08.05.2012                            &*
*& Sara Oikawa     DEVK9A0MDO   10.08.2020   38859 - Melhorias        &*
*& Sara Oikawa     DEVK9A0NM6   07.10.2020   Melhorias Pacote 4       &*
*& Sara Oikawa     DEVK9A0OEU   19.10.2020   Melhorias Pacote 5/6     &*
*& Sara Oikawa     DEVK9A0PEK   19.11.2020   44164 Melhorias Pacote 7 &*
*& Sara Oikawa     DEVK9A0PEK   19.11.2020   39798 Melhorias Pacote 8 &*
*&--------------------------------------------------------------------&*

REPORT  zsdr016_backup_21122023.
INCLUDE <icon>.
INCLUDE <cl_alv_control>.
TYPE-POOLS: vrm, ustyp, slis, f4typ.

TABLES: spell, zsdt0159.

DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.
DATA: wl_mode(1).

TYPES: BEGIN OF ty_itens,
*         DOC_SIMULACAO TYPE ZSDT0041-DOC_SIMULACAO,
         posnr         TYPE zsdt0041-posnr,
         auart         TYPE zsdt0041-auart,
         spart         TYPE zsdt0041-spart,
         inco1         TYPE zsdt0041-inco1,
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
         cultura_apl   TYPE zsdt0041-cultura_apl,
         safra_apl     TYPE zsdt0041-safra_apl,
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
         matnr         TYPE zsdt0041-matnr,
         maktx         TYPE makt-maktx,
         werks_fornec  TYPE zsdt0041-werks_fornec,
         werks         TYPE zsdt0041-werks,
         charg         TYPE zsdt0041-charg,
         zmeng         TYPE zsdt0041-zmeng,
         zieme         TYPE zsdt0041-zieme,
         dtvenc        TYPE zsdt0041-dtvenc,
         desconto      TYPE zsdt0041-desconto,
*         DESC_ABSOLUTO TYPE ZSDT0041-DESC_ABSOLUTO,
         desc_absoluto TYPE  zsdt0094-total_proporc,
         vlr_ajuste    TYPE  zsdt0094-total_proporc,
         vl_unit       TYPE p DECIMALS 2,
*         ZWERT         TYPE ZSDT0041-ZWERT,
         zwert         TYPE p DECIMALS 2,
         calcu         TYPE zsdt0041-calcu,
         trunit        TYPE zsdt0041-trunit,
         siumb         TYPE zsdt0041-siumb,
         trtot         TYPE zsdt0041-trtot,
         compr         TYPE zsdt0041-compr,
         mgcad         TYPE zsdt0041-mgcad,
         mgefe         TYPE zsdt0041-mgefe,
         vlr_frete     TYPE zsdt0041-vlr_frete,
*         VLRTOT        TYPE ZSDT0041-VLRTOT,
         vlrtot        TYPE p DECIMALS 2,
         style         TYPE lvc_t_styl,
         vbeln         TYPE vbak-vbeln,
         vlr_icms      TYPE zsdt0041-vlr_icms,
         vlr_cofins    TYPE zsdt0041-vlr_cofins,
         vlr_pis       TYPE zsdt0041-vlr_pis,
         j_1btxsdc     TYPE zsdt0008-j_1btxsdc,
         j_1btaxlw1    TYPE zsdt0008-j_1btaxlw1,
         j_1btaxlw5    TYPE zsdt0008-j_1btaxlw5,
         j_1btaxlw4    TYPE zsdt0008-j_1btaxlw4,
         zwert_liqdo   TYPE zsdt0041-zwert_liqdo,
       END OF ty_itens,

       BEGIN OF ty_antec,
         mark(1),
         vbeln                   TYPE zsdt0159-vbeln,
         zid_lanc                TYPE zsdt0159-zid_lanc,
         adiant                  TYPE zsdt0159-adiant,
         gjahr                   TYPE zsdt0159-gjahr,
         mont_moeda              TYPE zsdt0159-mont_moeda,
         augbl                   TYPE bsad_view-augbl,
         augdt                   TYPE bsad_view-augdt,
         usnam                   TYPE zsdt0159-usnam,
         data_atual              TYPE zsdt0159-data_atual,
         usnam_e                 TYPE zsdt0159-usnam_e,
         data_atual_e            TYPE zsdt0159-data_atual_e,
         " 21.02.2023 - RAMON - 102323 -->
         id_transacao_financeira TYPE zsdt0159-id_transacao_financeira,
         " 21.02.2023 - RAMON - 102323 --<
         obj_key                 TYPE zsdt0159-obj_key,
         estorno                 TYPE zsdt0159-estorno,
         bukrs                   TYPE zib_contabil-bukrs,
         seq                     TYPE zsdt0159-seq,
         line_color(4)           TYPE c,
       END OF ty_antec,

       BEGIN OF ty_infor,
         spart  TYPE tspat-spart,
         vtext  TYPE tspat-vtext,
         vlrtot TYPE zsdt0041-vlrtot,
       END OF ty_infor,

*       BEGIN OF TY_TRANS.
*         SEQUENCIA TYPE ZSDT0090-SEQUENCIA,
*         AUART     TYPE ZSDT0090-AUART,
*         VBELN     TYPE ZSDT0090-VBELN,
*         VBELV     TYPE ZSDT0090-VBELV,
*         ESTORNO   TYPE ZSDT0090-ESTORNO,
*       END OF TY_TRANS.

       BEGIN OF ty_formula,
         vlr_perc TYPE zsdt0042-vlr_perc,
         vlr_aliq TYPE zsdt0042-vlr_aliq,
       END OF ty_formula,

       BEGIN OF ty_memo,
         tante          TYPE zsdt0039-tx_juros,
         inss           TYPE zsdt0042-vlr_perc,
         facs(4)        TYPE p DECIMALS 2,
         pbrto          TYPE zsdt0040-prec_ant_cult,
         vlrant         TYPE zsdt0040-prec_ant_cult,
         vlrant_aux     TYPE zsded005,
         vlrinss        TYPE zsdt0040-prec_ant_cult,
         vlrinss_aux    TYPE zsded005,
         vlrfacs(4)     TYPE p DECIMALS 2,
         pliqdo         TYPE zsdt0040-prec_ant_cult,
         pliqdo_aux     TYPE zsded005,
         quant          TYPE zsdt0040-trototsc,
         vta_bruto      TYPE p DECIMALS 2,
*#138092 - ITSOUZA - 21.05.2024 - Inicio
         fundeinfra     TYPE zsdt0042-vlr_perc,
         vlrfundein     TYPE zsdt0040-prec_ant_cult,
         vlrfundein_aux TYPE zsded005,
*#138092 - ITSOUZA - 21.05.2024 - Fim

       END OF ty_memo,

       BEGIN OF ty_mglobal,
         matkl TYPE t023t-wgbez,
         mgcad TYPE zsdt0041-mgcad,
         mgefe TYPE zsdt0041-mgefe,
       END OF ty_mglobal,

       BEGIN OF ty_matnr,
         mark,
         matnr        TYPE mara-matnr,
         maktx        TYPE makt-maktx,
         meins        TYPE zsdt0036-meins,
         werks_fornec TYPE zsdt0036-werks_fornec,
         inco1        TYPE zsdt0036-inco1,
         safra        TYPE zsdt0036-safra,
         auart        TYPE zsdt0041-auart,
         spart        TYPE zsdt0041-spart,
         charg        TYPE zsdt0041-charg,
         dtvenc       TYPE zsdt0041-dtvenc,
       END OF ty_matnr .

* Saida para o contrato
TYPES: BEGIN OF ty_saida,
         v_vbeln_e      TYPE vbeln,
         v_vbeln_i      TYPE c LENGTH 200,

         "DESCRIÇÃO DO USUARIOS COMPANHEIRA DOCUMENTOS E ENDEREÇO.
         id_nome        TYPE kunnr, " Id do Cliente
         nome           TYPE name1_gp, " nome do Cliente
         rg             TYPE stcd3, " RG do Cliente
         cpf            TYPE stcd2, " CPF do Cliente
         id_nome_c      TYPE konzs, " Id do Companheiro do Cliente
         nome_c         TYPE name1_gp, " Nome do companheiro do cliente
         rg_c           TYPE stcd3, " RG do companheiro do Cliente
         cpf_c          TYPE stcd2, " CPF do companheiro do Cliente
         rua            TYPE stras_gp, " Endereço do Cliente
         cidade         TYPE mcdd3, " Cidade do Cliente
         uf             TYPE regio, " UF do Cliente
         estado         TYPE bezei20, " Estado do Cliente
*        ZSDT0041
         auart          TYPE auart, " Tipo de Mercadoria
*        ZSDT0040
         tpsim          TYPE zsdt0040-tpsim, " Tipo de transação
         waerk          TYPE waerk, " tipo moeda
*      REAIS       TYPE DMBTR, " valor moeda Reais
         reais          TYPE zsded005, " valor moeda Reais
         reais_desc     TYPE char200, " desc moeda Reais

         dolar          TYPE zsded005, " valor moeda Dolar
         dolar_desc     TYPE char200, " Desc moeda Dolar

         venc           TYPE bapi_jbd_dte_dzterm, " vencimento moeda
         venc_dia       TYPE c LENGTH 2, " vencimento moeda Dia
         venc_mes       TYPE c LENGTH 2, " vencimento moeda Mes
         venc_ano       TYPE c LENGTH 4, " vencimento moeda Ano


         juros          TYPE zsded002,
         juros_desc     TYPE char200,
         pag_prorrogado TYPE char1, " Pagamento prorrogado
         juros_dia      TYPE n LENGTH 2,
         juros_mes      TYPE n LENGTH 2,
         juros_ano      TYPE n LENGTH 4,

         icon           TYPE inco1,
         local          TYPE zsdt0040-fazenda,
         local1         TYPE ort02,
         werks          TYPE werks,
*        Dados Fiadores
         f1             TYPE zsdt0040-fiador_01,
         f1_name        TYPE name1_gp,
         prof_f1        TYPE zsdt0040-fiador_01,
         rg_f1          TYPE stcd3,
         orgao_rg_f1    TYPE zsdt0040-fiador_01,
         cpf_f1         TYPE stcd2,
         id_f1_c        TYPE kunnr,
         f1_c           TYPE name1_gp,
         prof_f1_c      TYPE zsdt0040-fiador_01,
         rg_f1_c        TYPE stcd3,
         orgao_rg_f1_c  TYPE zsdt0040-fiador_01,
         cpf_f1_c       TYPE stcd2,

         end_f1         TYPE stras_gp,
         cidade_f1      TYPE mcdd3,
         uf_f1          TYPE regio,
         estado_f1      TYPE bezei20, " Estado do F1

         f2             TYPE zsdt0040-fiador_02,
         f2_name        TYPE name1_gp,
         prof_f2        TYPE zsdt0040-fiador_02,
         rg_f2          TYPE stcd3,
         orgao_rg_f2    TYPE zsdt0040-fiador_02,
         cpf_f2         TYPE stcd2,
         id_f2_c        TYPE kunnr,
         f2_c           TYPE name1_gp,
         prof_f2_c      TYPE zsdt0040-fiador_02,
         rg_f2_c        TYPE stcd3,
         orgao_rg_f2_c  TYPE zsdt0040-fiador_02,
         cpf_f2_c       TYPE stcd2,

         end_f2         TYPE stras_gp,
         cidade_f2      TYPE mcdd3,
         uf_f2          TYPE regio,
         estado_f2      TYPE bezei20, " Estado do F2

         dia            TYPE c LENGTH 2,
         mes            TYPE c LENGTH 2,
         ano            TYPE c LENGTH 4,
         s_c            TYPE c LENGTH 1,
         s_f1           TYPE c LENGTH 1,
         s_f2           TYPE c LENGTH 1,
         dt_entrega_sem TYPE epsdeldat,
         dt_entrega_def TYPE epsdeldat,
         dt_entrega_fet TYPE epsdeldat,

       END OF ty_saida.

TYPES: tg_antec_f         TYPE STANDARD TABLE OF ty_antec.

* Declarações do Contrato
DATA: it_contrato  TYPE TABLE OF zsdt0040,
      wa_contrato  TYPE zsdt0040,
      it_itens     TYPE TABLE OF zsdt0041,
      it_itens_aux TYPE TABLE OF zsdt0041,
      wa_itens     TYPE zsdt0041,
      it_kna1      TYPE TABLE OF kna1,
      wa_kna1      TYPE kna1,
      it_kna1_c    TYPE TABLE OF kna1,
      wa_kna1_c    TYPE kna1,
      it_kna1_f    TYPE TABLE OF kna1,
      wa_kna1_f    TYPE kna1,
      it_makt      TYPE TABLE OF makt,
      wa_makt      TYPE makt,
      it_saida     TYPE TABLE OF ty_saida,
      wa_saida     TYPE ty_saida,
      convert      TYPE n LENGTH 10,
      wa_import    TYPE zcontrato_insumos,
*      WA_IMPORT   TYPE TY_SAIDA,
      c_vbeln      TYPE c LENGTH 255,
      taxa         TYPE ukurs_curr.

DATA: wa_t001k     TYPE t001k.

DATA: valor(8) TYPE p DECIMALS 2.

* Estrutura para Palavras por extenso
DATA: BEGIN OF palavra,
        inteiro     LIKE spell-word,
        real(6),
        dolar(7),
        filler(3),
        decimal     LIKE spell-decword,
        centavos(8),
      END OF palavra.

DATA: decimals TYPE p.
DATA word LIKE spell.
DATA gv_info.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_zsdt0090.
         INCLUDE TYPE zsdt0090.
TYPES color(4).
TYPES: END OF ty_zsdt0090.

TYPES: BEGIN OF t_cursor,              "Typ für Cursor-Position
         fname LIKE d021s-fnam,        "Feldname
         pos   LIKE sy-stepl,            "Loop-Zeile auf akt. Seite
         value LIKE d021s-fnam,        "Inhalt des Dynprofeldes
         tc    LIKE dd04l-rollname,       "Table-Control-Name (tc+)
         tcsec LIKE dd04l-rollname,    "TC-Zusatzattr.name (tc+_sec)
         line  LIKE sy-stepl,           "Zeile in ITAB
       END OF t_cursor.

TYPES: BEGIN OF ty_itens_id,
         index   TYPE i,
         doc     TYPE zsdt0041-doc_simulacao,
         vbeln   TYPE zsdt0041-vbeln,
         posnr   TYPE zsdt0041-posnr,
         vlr_ini TYPE zsded005,
         vlr_atu TYPE zsded005,
         vlr_dif TYPE zsded005,
       END OF ty_itens_id.


TYPES: BEGIN OF ty_print,
         dt_entrega_sem TYPE zsdt0040-dt_entrega_sem,
         dt_entrega_def TYPE zsdt0040-dt_entrega_def,
         dt_entrega_fet TYPE zsdt0040-dt_entrega_fet,
         juros_mora     TYPE zsdt0040-juros_mora,
         fiador_01      TYPE zsdt0040-fiador_01,
         fiador_02      TYPE zsdt0040-fiador_02,
         checkyes       TYPE c,
         checkno        TYPE c,
         checkyes1      TYPE c,
         checkno1       TYPE c,
       END OF ty_print.

TYPES: BEGIN OF ty_log_acao,
         id_historico  TYPE zsdt0186-id_historico,
         doc_simulacao TYPE zsdt0186-doc_simulacao,
         status        TYPE zsdt0186-status,
         motivo        TYPE zsdt0186-motivo,
         usnam         TYPE zsdt0186-usnam,
         data_atual    TYPE zsdt0186-data_atual,
         hora_atual    TYPE zsdt0186-hora_atual,
       END OF ty_log_acao.


DATA: wa_print    TYPE ty_print,
      it_0041     TYPE TABLE OF zsdt0040,
      wa_0041     TYPE zsdt0040,
      it_desabs   TYPE TABLE OF ty_itens WITH HEADER LINE,
      vta_sistpro TYPE p DECIMALS 2,
      vlr_total   TYPE p DECIMALS 2,
*      MEIO_PAGO TYPE ZSDT0040-MEIO_PAGO.
      meio_pago   TYPE zsded049.

* Início - Sara Oikawa - 38859 - Agosto/2020
DATA: wg_meio_pago   TYPE char1,
      wg_flg_receb   TYPE flag,
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
      wg_cultura_apl TYPE zsdt0038-cultura.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

*Fim - Sara Oikawa - 38859 - Agosto/2020

* Início - Sara Oikawa - 19.10.2020   Melhorias Pacote 5/6
TYPES: BEGIN OF ty_sigam,
         mark,
         doc_simulacao     TYPE zsdt0260-doc_simulacao,
         id_compra         TYPE zsdt0260-id_compra,
         safra             TYPE zsdt0260-safra,
         status            TYPE zsdt0260-status,
         id_filial_sap     TYPE zsdt0260-id_filial_sap,
         id_material_sap   TYPE zsdt0260-id_material_sap,
         id_cliente_sap    TYPE zsdt0260-id_cliente_sap,
         id_fornec_sap     TYPE zsdt0260-id_fornec_sap,
         compra_fim_export TYPE zsdt0260-compra_fim_export,
         data_compra       TYPE zsdt0260-data_compra,
       END OF ty_sigam,

       BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         sperr TYPE lfa1-sperr,
         sperm TYPE lfa1-sperm,
         sperq TYPE lfa1-sperq,
       END   OF ty_lfa1.

DATA: tg_sigam           TYPE TABLE OF ty_sigam,
      wg_sigam           TYPE ty_sigam,

      tg_0260            TYPE zsdt0260_t,
      wg_0260            TYPE zsdt0260,

      tg_lfa1            TYPE TABLE OF ty_lfa1,
      wg_lfa1            TYPE ty_lfa1,

      tg_funex           TYPE TABLE OF zsdt0001funex,
      wg_funex           TYPE zsdt0001funex,

      wg_cnpj            TYPE stcd1,
      wg_cpf             TYPE stcd2,

      wg_safra_0110_sg   TYPE ajahr,
      wg_idcompr_0110_sg TYPE zde_id_compra_sg.

* Fim - Sara Oikawa - 19.10.2020   Melhorias Pacote 5/6


DATA: wg_header     TYPE zsdt0040,
      wg_header_old TYPE zsdt0040.

DATA: btn_fi TYPE c.

DATA: w_answer(1),
      wl_lines      TYPE sy-tabix,
      tl_index_rows TYPE lvc_t_row,
      wl_index_rows TYPE lvc_s_row.
*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: c_x              TYPE c VALUE 'X',
           c_a              TYPE c VALUE 'A',
           c_r              TYPE c VALUE 'R',
           c_b              TYPE c VALUE 'B',
           c_t              TYPE c VALUE 'T',
           c_v              TYPE c VALUE 'V',
           c_p              TYPE c VALUE 'P',
           c_tro(3)         TYPE c VALUE 'TRO',
           c_vis(3)         TYPE c VALUE 'VIS',
           c_adt(3)         TYPE c VALUE 'ADT',
           c_add(3)         TYPE c VALUE 'ADD',
           c_del(3)         TYPE c VALUE 'DEL',
           c_exit(4)        TYPE c VALUE 'EXIT',
           c_back(4)        TYPE c VALUE 'BACK',
           c_copy(4)        TYPE c VALUE 'COPY',
           c_save(4)        TYPE c VALUE 'SAVE',
           c_bloq(4)        TYPE c VALUE 'BLOQ',
           c_memo(4)        TYPE c VALUE 'MEMO',
           c_clos_msg(8)    TYPE c VALUE 'CLOS_MSG',
           c_mglob(5)       TYPE c VALUE 'MGLOB',
           c_gera(5)        TYPE c VALUE 'GERA',
           c_atual(5)       TYPE c VALUE 'ATUAL',
           c_print(5)       TYPE c VALUE 'PRINT',
           c_printc(6)      TYPE c VALUE 'PRINTC',
           c_modif(5)       TYPE c VALUE 'MODIF',
           c_sacas(8)       TYPE c VALUE 'QTD_SACA',
           c_dsc_abs(7)     TYPE c VALUE 'DSC_ABS',
           c_col_exp(7)     TYPE c VALUE 'COL_EXP',
           c_vinc_desc(9)   TYPE c VALUE 'VINC_DESC',
           c_aprov(5)       TYPE c VALUE 'APROV',
*Início - Sara Oikawa - 38859 - Agosto/2020
           c_altjur(6)      TYPE c VALUE 'ALTJUR',
           c_altadt(6)      TYPE c VALUE 'ALTADT',
*Fim - Sara Oikawa - 38859 - Agosto/2020
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
           c_altcsa(6)      TYPE c VALUE 'ALTCSA',
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
           c_btn_sg(6)      TYPE c VALUE 'BTN_SG',
           c_s              TYPE c VALUE 'S',
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
           c_altcpg(6)      TYPE c VALUE 'ALTCPG',
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

*           c_red(4)         type c value '@0A@',
*           c_yellow(4)      type c value '@09@',
*           c_green(4)       type c value '@08@',
*           c_aguard(4)      type c value '@9R@',

           " 05.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- >
           c_boleta_vin(10) TYPE c VALUE 'BOLETA_VIN',
           " 05.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- <

           c_tpord(5)       TYPE c VALUE 'TPORD',
           c_descp(5)       TYPE c VALUE 'DESCP',
           c_proces(6)      TYPE c VALUE 'PROCES',
           c_cancel(6)      TYPE c VALUE 'CANCEL',
           c_atuali(6)      TYPE c VALUE 'ATUALI',
           c_search(6)      TYPE c VALUE 'SEARCH',
           c_reprov(6)      TYPE c VALUE 'REPROV',
           c_descont(7)     TYPE c VALUE 'DESCONT',
           c_show_msgre(10) TYPE c VALUE 'SHOW_MSGRE',
           c_dp_click(8)    TYPE c VALUE 'DP_CLICK',
           c_mat(3)         TYPE c VALUE 'MAT',
           c_show_log(9)    TYPE c VALUE 'SHOW_LOG',
           c_recalc(6)      TYPE c VALUE 'RECALC'.

*&--------------------------------------------------------------------&*
*& Declaração de Variaveis/Tabelas/Workarea                           &*
*&--------------------------------------------------------------------&*
DATA:
*       WG_HEADER          TYPE ZSDT0040,
  tg_itens           TYPE TABLE OF ty_itens WITH HEADER LINE,
  tg_antec           TYPE TABLE OF ty_antec WITH HEADER LINE,

  it_itens_id        TYPE TABLE OF ty_itens_id,
  wa_itens_id        TYPE ty_itens_id,
  tg_infor           TYPE TABLE OF ty_infor WITH HEADER LINE,
*  TG_TRANS           TYPE TABLE OF TY_TRANS WITH HEADER LINE,
*  TG_TRANS           TYPE TABLE OF ZSDT0090 WITH HEADER LINE,
  tg_trans           TYPE TABLE OF ty_zsdt0090 WITH HEADER LINE,

  tg_setleaf         TYPE TABLE OF setleaf WITH HEADER LINE,
  tg_setlinet        TYPE TABLE OF setlinet WITH HEADER LINE,
  tg_setleaf_cult    TYPE TABLE OF setleaf WITH HEADER LINE,
  tg_setlinet_cult   TYPE TABLE OF setlinet WITH HEADER LINE,
  it_zsdt0041_ajuste TYPE STANDARD TABLE OF zsdt0041,
  wg_desc_kunnr      TYPE kna1-name1,
*Início - Sara Oikawa - 38859 - Agosto/2020
  wg_desc_emissor    TYPE char80,
  wg_desc_ort01      TYPE kna1-ort01,
  wg_desc_regio      TYPE kna1-regio,
*Fim - Sara Oikawa - 38859 - Agosto/2020
  wg_desc_vkbur      TYPE t001w-name1,
  wg_desc_waerk      TYPE tcurt-ktext,
  wg_desc_cultura    TYPE zsdt0038-descricao,
  wg_memo            TYPE ty_memo,
  wg_prop            TYPE ty_memo,
  tg_mglobal         TYPE TABLE OF ty_mglobal WITH HEADER LINE,
  tg_matnr           TYPE TABLE OF ty_matnr WITH HEADER LINE,
  tg_log_acao        TYPE zsdt0186,
  tg_log             TYPE TABLE OF zsdt0186 WITH HEADER LINE,

*Início - Sara Oikawa - 38859 - Agosto/2020
  tg_log_edicao      TYPE TABLE OF zsdt0091,
  wg_log_edicao      TYPE zsdt0091,

  wl_dtlimite        TYPE datum,
  wl_qtd_diasu       TYPE i,
*Fim - Sara Oikawa - 38859 - Agosto/2020

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
  wg_matnr           TYPE mara-matnr,
  wg_groes           TYPE mara-groes,
  wg_meins           TYPE mara-meins,
  wg_groes_string    TYPE char30,
  wg_qtd_prop        TYPE zsdt0041-zmeng,
  wg_groes_dec       TYPE esecompavg,
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  wg_tpsim_ant       TYPE zsdt0040-tpsim,
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  wg_desc_hbkid(50), "US - 81799 - CSB
  wg_desc_tpcult(30),
  wg_desc_vendedor   TYPE tvgrt-bezei,
  wa_style           TYPE lvc_s_styl,
  style              TYPE lvc_t_styl WITH HEADER LINE,
  wg_desc_dat(30)    VALUE 'Dt.Pagamento',
  wg_acao(10),
  s_acao(10),
  wg_save(10),
  wg_copy(01),
  wg_display,
  x_field(30),
  wg_mensagem(30),
  wg_obj(40),
  wg_flag,
  wg_area_ha         TYPE i,
  wg_status(4),
  wg_colaps(4)       VALUE '@K2@',
  init,
  wl_dias_vct        TYPE i,
  wl_juro_dias(7)    TYPE p DECIMALS 5,
  ok_code            TYPE sy-ucomm,
  fiador_01          TYPE name1,
  fiador_02          TYPE name1,
  tpsim              TYPE c LENGTH 2,
  c_erro             TYPE sy-subrc,
  convert_tratotsc   TYPE i,
  total_sac          TYPE i,
  _hoje_invdt        TYPE sy-datum,
  sum_vlrtot         TYPE zsdt0040-vlrtot,
  convert_texto      TYPE sytitle.

*US 143453 - INICIO - PQ
  DATA:  lt_vkorg TYPE TABLE OF rgsb4,
         w_vkorg    TYPE rgsb4.

  RANGES: r_vkorg    FOR ZSDT0040-VKORG.
*US 143453 - FIM - PQ

* Variaveis para o PopUp do Desconto
DATA: desconto TYPE p DECIMALS 2.
DATA: total TYPE p DECIMALS 2.
DATA: desc_acres TYPE c LENGTH 15.

" 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
DATA gs_0308 TYPE zsdt0308.
DATA gv_taxa_neg TYPE ukursp.
" 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

DATA: taxa_fre TYPE zsdt0117-kursk.
*RSI - Case #CS0971604 Ajuste divisão frete
DATA: erro_taxa_frete TYPE bukrs.
*RSI - Case #CS0971604 Ajuste divisão frete
DATA: BEGIN OF gt_values OCCURS 0,
        domvalue_l TYPE domvalue_l,
        ddtext     TYPE val_text,
      END OF gt_values.

DATA: tg_msg_ret TYPE TABLE OF zfiwrs0002 WITH HEADER LINE,
      t_usermd   TYPE STANDARD TABLE OF rgsb4 WITH HEADER LINE.

DATA: ty_toolbar TYPE stb_button.

DATA: wg_tvkot TYPE tvkot,
      wg_tvtwt TYPE tvtwt,
      wg_tvtw  TYPE tvtw,
      wg_tspat TYPE tspat,
      wg_tvkbt TYPE tvkbt,
      wg_tvgrt TYPE tvgrt.

*&--------------------------------------------------------------------&*
*& Declaração de Variaveis para SHDB                                  &*
*&--------------------------------------------------------------------&*
DATA: ti_bdcdata       TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      wa_bdcdata       LIKE LINE OF ti_bdcdata,
      t_messtab        TYPE TABLE OF bdcmsgcoll,
      wg_documento(10).

DATA: wg_bdc TYPE bdcdata,
      tg_bdc TYPE TABLE OF bdcdata,
      tg_msg TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
      wg_msg TYPE bdcmsgcoll.

*&--------------------------------------------------------------------&*
*& Paineis
*&--------------------------------------------------------------------&*
DATA: dynnr_top(4) TYPE n VALUE '0105'.

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
*Class definition for ALV toolbar
CLASS: lcl_alv_toolbar   DEFINITION DEFERRED.

DATA: grid1                TYPE REF TO cl_gui_alv_grid,
      container1           TYPE REF TO cl_gui_custom_container,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,

      grid2                TYPE REF TO cl_gui_alv_grid,
      g_container          TYPE scrfname VALUE 'CC_TRANS',
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      splitter             TYPE REF TO cl_gui_splitter_container,
      container_1          TYPE REF TO cl_gui_container,       "splitter conteiner 1

      grid3                TYPE REF TO cl_gui_alv_grid,
      container_3          TYPE REF TO cl_gui_custom_container,


      obj_grid_infor       TYPE REF TO cl_gui_alv_grid,
      obj_cont_infor       TYPE REF TO cl_gui_custom_container.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events TYPE slis_alv_event,
      events    TYPE slis_t_event,
      t_print   TYPE slis_print_alv,
      t_top     TYPE slis_t_listheader.

DATA: t_fieldcatalog   TYPE lvc_t_fcat,
      t_fieldcatalog2  TYPE lvc_t_fcat,
      t_fieldcat_infor TYPE lvc_t_fcat,
      w_fieldcatalog   TYPE lvc_s_fcat,
      tg_selectedcell  TYPE lvc_t_cell,
      wg_selectedcell  TYPE lvc_s_cell,
      wa_layout        TYPE lvc_s_layo,
      wa_stable        TYPE lvc_s_stbl VALUE 'XX',
      wg_cell          TYPE lvc_s_cell,
      tg_cell          TYPE lvc_t_cell.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid VALUE sy-repid.


SELECTION-SCREEN: BEGIN OF SCREEN 999.
  PARAMETERS p_vlr AS CHECKBOX.
SELECTION-SCREEN: END OF SCREEN 999.

SELECTION-SCREEN:  BEGIN OF SCREEN 0211 AS SUBSCREEN.
  PARAMETERS: sim RADIOBUTTON GROUP rd1 USER-COMMAND uc_s,
              nao RADIOBUTTON GROUP rd1.
  SELECTION-SCREEN SKIP.
  PARAMETERS: vlr AS CHECKBOX USER-COMMAND cb_v.
SELECTION-SCREEN:  END OF SCREEN 0211.

CALL SCREEN 100.

AT SELECTION-SCREEN.
  PERFORM check_campos.

AT SELECTION-SCREEN OUTPUT.
  PERFORM check_campos.

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.

    CLASS-METHODS:
      on_hotspot_click_tra FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data
                  et_bad_cells e_display.


ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: wl_desactive.

    wl_desactive = space.
    IF sy-dynnr = '0108'.

**   variable for Toolbar Button
*      TY_TOOLBAR-ICON      = ICON_VIEW_CLOSE.
*      TY_TOOLBAR-FUNCTION  = C_CLOS_MSG.
*      TY_TOOLBAR-DISABLED  = SPACE.
*      TY_TOOLBAR-BUTN_TYPE = 0.
*      APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*      CLEAR TY_TOOLBAR.

    ELSE.
      ty_toolbar-icon      =  icon_insert_row.
      ty_toolbar-function  =  c_add.
      IF wg_acao EQ c_add
      OR wg_acao EQ c_modif
      OR wg_acao EQ c_descont.
        LOOP AT tg_itens TRANSPORTING NO FIELDS
            WHERE vbeln IS NOT INITIAL.

        ENDLOOP.
        IF sy-subrc IS INITIAL.
          ty_toolbar-disabled  = 1.
        ELSE.
          ty_toolbar-disabled  = space.
        ENDIF.
      ELSE.
        ty_toolbar-disabled  = 1.
      ENDIF.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
      IF wg_save EQ c_altcpg.
        ty_toolbar-disabled  = 1.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.


*    IF WG_DOCS-DOCNUM IS INITIAL.
*      READ TABLE TG_FIELDS TRANSPORTING NO FIELDS
*        WITH KEY GROUP1 = 'GR1'.
*      IF SY-SUBRC IS INITIAL.
*        WL_DESACTIVE = SPACE.
*      ELSE.
*        WL_DESACTIVE = 1.
*      ENDIF.
*    ELSE.
*      WL_DESACTIVE = 1.
*    ENDIF.
      ty_toolbar-icon      =  icon_delete_row.
      ty_toolbar-function  =  c_del.
      IF wg_acao EQ c_add
      OR wg_acao EQ c_modif
      OR wg_acao EQ c_descont.
        LOOP AT tg_itens TRANSPORTING NO FIELDS
            WHERE vbeln IS NOT INITIAL.

        ENDLOOP.
        IF sy-subrc IS INITIAL.
          ty_toolbar-disabled  = 1.
        ELSE.
          ty_toolbar-disabled  = space.
        ENDIF.

      ELSE.
        ty_toolbar-disabled  = 1.
      ENDIF.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
      IF wg_save EQ c_altcpg.
        ty_toolbar-disabled  = 1.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

*&-------------------------------------------------------------------&*
*& Modificação - Eduardo Ruttkowski Tavares - 14.11.2013 >>> INI     &*
*& CH 114788 - Simulador de Vendas                                   &*
*&-------------------------------------------------------------------&*
      ty_toolbar-butn_type = 3.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_select_block.
      ty_toolbar-function  =  c_mat.
      IF wg_acao EQ c_add
      OR wg_acao EQ c_modif
      OR wg_acao EQ c_descont.
        LOOP AT tg_itens TRANSPORTING NO FIELDS
            WHERE vbeln IS NOT INITIAL.

        ENDLOOP.
        IF sy-subrc IS INITIAL.
          ty_toolbar-disabled  = 1.
        ELSE.
          ty_toolbar-disabled  = space.
        ENDIF.
      ELSE.
        ty_toolbar-disabled  = 1.
      ENDIF.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
      IF wg_save EQ c_altcpg.
        ty_toolbar-disabled  = 1.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

*&-------------------------------------------------------------------&*
*& Modificação - Eduardo Ruttkowski Tavares - 14.11.2013 >>> END     &*
*&-------------------------------------------------------------------&*
*
*    TY_TOOLBAR-BUTN_TYPE = 3.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.
**   variable for Toolbar Button
*    TY_TOOLBAR-ICON      =  ICON_VIEW_CLOSE.
*    TY_TOOLBAR-FUNCTION  =  C_CLOS_MSG.
*    TY_TOOLBAR-DISABLED  = SPACE.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.
**   Call reorganize method of toolbar manager to
**   display the toolbar

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.
    DATA: tl_itens_aux LIKE TABLE OF tg_itens,
          wl_itens     LIKE LINE OF tg_itens,
          wl_lines     TYPE sy-tabix.
    REFRESH: tl_itens_aux.

**   User Command Botões Incluidos
*    IF P_OPERACAO IS  NOT INITIAL
*AND P_BUKRS IS    NOT INITIAL
* AND P_BRANCH IS  NOT INITIAL
*  AND P_PARVW IS  NOT INITIAL
*   AND P_PARID IS NOT INITIAL.
    CASE e_ucomm.
*        WHEN C_CLOS_MSG.
*          IF GRID2 IS NOT INITIAL.
*            CALL METHOD GRID2->FREE.
*            FREE: CONTAINER_2, GRID2.
*          ENDIF.
**    posiciona spliter na altura x
*          IF SPLITTER IS NOT INITIAL.
*            CALL METHOD SPLITTER->SET_ROW_HEIGHT
*              EXPORTING
*                ID     = 1
*                HEIGHT = 100.
*          ENDIF.
*          LEAVE TO SCREEN 100.
      WHEN c_add.
*        APPEND INITIAL LINE TO TG_SAIDA.

        IF wg_header-tpsim IS NOT INITIAL.
          tl_itens_aux[] = tg_itens[].
          REFRESH: tg_itens.
          LOOP AT tl_itens_aux INTO wl_itens.
            wl_itens-posnr = sy-tabix * 10.
            APPEND wl_itens TO tg_itens.
          ENDLOOP.
          DESCRIBE TABLE tg_itens LINES wl_lines.
          CLEAR: wl_itens.
          wl_itens-posnr = ( wl_lines + 1 ) * 10 .

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
          IF wg_cultura_apl IS NOT INITIAL.
            wl_itens-cultura_apl = wg_cultura_apl.
          ENDIF.

          wl_itens-safra_apl = wg_header-safra.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
          APPEND wl_itens TO tg_itens.

        ENDIF.
        CALL METHOD grid1->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      WHEN c_del.

        CALL METHOD grid1->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.
          DELETE tg_itens INDEX wg_selectedcell-row_id-index.

        ENDLOOP.

        PERFORM vlrtot.

*        CALL SCREEN 0100.

        CALL METHOD grid1->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

*&-------------------------------------------------------------------&*
*& Modificação - Eduardo Ruttkowski Tavares - 14.11.2013 >>> INI     &*
*& CH 114788 - Simulador de Vendas                                   &*
*&-------------------------------------------------------------------&*
      WHEN c_mat.
        PERFORM vlrtot.
        PERFORM display_matnr.
*&-------------------------------------------------------------------&*
*& Modificação - Eduardo Ruttkowski Tavares - 14.11.2013 >>> END     &*
*&-------------------------------------------------------------------&*
    ENDCASE.
*    PERFORM VERIFICA_ERROS.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen   = '100'
        i_show     = space
        i_repid    = sy-repid
        i_popup    = 0
*       i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*       I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        e_messagem = wg_mensagem
      TABLES
        it_msgs    = tg_msg_ret.

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

*    ENDIF.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD on_double_click.
*    data: wl_itens like line of tg_itens,
*          tl_impo_aux like table of tg_impo.
*
*
*    if e_row gt 0.
*      read table tg_itens into wl_itens index e_row.
*      if wl_itens-matnr is not initial
*      and wl_itens-werks is not initial
*      and wl_itens-menge is not initial
*      and wl_itens-netpr is not initial.
**    posiciona spliter na altura x
*        call method splitter->set_row_height
*          exporting
*            id     = 1
*            height = 50.
*
*        vg_subscreen1 = c_dummy_header.
*
*        if grid2 is not initial.
*          call method grid2->free.
*
*        endif.
*
*        free: container_2, grid2, tl_impo_aux.
*
*        call method splitter->get_container
*          exporting
*            row       = 2
*            column    = 1
*          receiving
*            container = container_2.
*        if grid2 is initial.
*          wa_layout-no_toolbar = c_x.
*          create object grid2
*            exporting
*              i_parent = container_2.
*
*          wa_layout-cwidth_opt = c_x.
**          wa_layout-grid_title = 'Impostos'.
*          condense e_row no-gaps.
*          concatenate 'Impostos do Item' '-' wl_itens-itmnum into wa_layout-grid_title separated by space.
*          perform montar_layout_impostos.
*          perform monta_impostos tables tl_impo_aux
*                                 using e_row.
*          call method grid2->set_table_for_first_display
*            exporting
*              is_layout       = wa_layout
*            changing
*              it_fieldcatalog = t_fieldcatalog[]
*              it_outtab       = tl_impo_aux[].
*
**      *** Método de atualização de dados na Tela
*          call method grid2->refresh_table_display
*            exporting
*              is_stable = wa_stable.
*        else.
**      *** Método de atualização de dados na Tela
*          call method grid2->refresh_table_display
*            exporting
*              is_stable = wa_stable.
*
*        endif.
*        wg_dg1 = c_maximizar.
*        leave to screen 100.
*      else.
**    posiciona spliter na altura x
*        call method splitter->set_row_height
*          exporting
*            id     = 1
*            height = 100.
*      endif.
*    endif.
*
*** Método de atualização de dados na Tela
**    CALL METHOD GRID2->REFRESH_TABLE_DISPLAY.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD on_data_changed.
    DATA: ls_good         TYPE lvc_s_modi,
*            LV_VALUE        TYPE,
          vl_tabix        TYPE sy-tabix,
          vl_value        TYPE lvc_value,
          wl_mara         TYPE mara,
          wl_makt         TYPE makt,
          wl_0036         TYPE zsdt0036,
          wl_0039         TYPE zsdt0039,
          wl_calculo      TYPE zsdt0041-calcu,
          wl_tunit        TYPE zsded005,  "   type zsdt0041-trunit,
          wl_prunit       TYPE zsded005,  "type zsdt0041-zwert,
          wl_dtvenc       TYPE zsdt0041-dtvenc,
          wl_trtot        TYPE zsded005,  "type zsdt0041-trtot,
          wl_zmeng        TYPE zsdt0041-zmeng,
          wl_desc_abs     TYPE zsdt0041-zmeng,
          wl_vlrtot       TYPE zsded005,  "type zsdt0041-vlrtot,
          wl_compr        TYPE zsdt0041-compr,
          wl_matnr        TYPE zsdt0041-matnr,
          wl_auart        TYPE vbak-auart,
          wl_0042         TYPE zsdt0042,
          tl_0042         TYPE TABLE OF zsdt0042,
          wl_vlr_perc     TYPE zsdt0042-vlr_perc,
          wl_vlr_aliq     TYPE zsdt0042-vlr_aliq,
          wl_tot_vlr      TYPE zsdt0041-zwert,
          wl_tot_vlr_unit TYPE zsdt0041-zwert,
          wl_xa           TYPE zsdt0041-zwert,
          wl_xb           TYPE zsdt0041-zwert,
          wl_xc           TYPE zsdt0041-zwert,
          wl_xd           TYPE zsdt0041-zwert,
          wl_inco1        TYPE zsdt0041-inco1,
          wl_werks        TYPE zsdt0041-werks,
          wl_setleaf      TYPE setleaf,
          wl_setlinet     TYPE setlinet,
          wl_desconto     TYPE zsdt0041-desconto,
*          WL_NEGOCIADO    TYPE ZSDED005,
          wl_negociado    TYPE p DECIMALS 2,
          wl_mgefe_aux    TYPE zsded005,
          wl_mgefe        TYPE zsdt0041-mgefe,
          wl_calc_aux     TYPE zsdt0041-calcu,
          wl_kna1         TYPE kna1,
          wl_zieme        TYPE zsdt0041-zieme,
          wl_zsdt0087     TYPE zsdt0087,
          wl_0037         TYPE zsdt0037,
          c_desc          TYPE c LENGTH 1,
          wl_itens        LIKE LINE OF tg_itens.
**** CS2022000324
    DATA: r_werks   TYPE RANGE OF werks_d,
          it_values TYPE TABLE OF rgsb4,
          wa_values TYPE rgsb4,
          wa_werks  LIKE LINE OF r_werks.
**** CS2022000324
    REFRESH: tl_0042.
    CLEAR:   wl_makt, wl_mara, wl_0036, wl_calculo,
             wl_tunit, wl_prunit, wl_0037, wl_0039, wl_trtot,
             wl_zmeng, wl_vlrtot, wl_matnr, wl_0042,
             wl_vlr_perc, wl_vlr_aliq, wl_tot_vlr, wl_tot_vlr_unit,
             wl_auart, tg_setlinet, tg_setleaf, wl_inco1, wl_werks,
             wl_setleaf,  wl_setlinet, wl_desconto, wl_mgefe_aux, wl_mgefe, wl_kna1, wl_zieme.
*
**** CS2022000324
    REFRESH it_values.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        setnr         = 'MV45AFZZ_WERKS'
        table         = 'VBAP'
        class         = '0000'
        fieldname     = 'WERKS'
      TABLES
        set_values    = it_values
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.
    IF sy-subrc IS INITIAL.

      LOOP AT it_values INTO wa_values.
        wa_werks = 'IEQ'.
        wa_werks-low    = wa_values-from.
        IF wa_values-to IS NOT INITIAL.
          wa_werks = 'IBT'.
          wa_werks-high = wa_values-to.
        ENDIF.

        APPEND wa_werks TO r_werks.
      ENDLOOP.
    ENDIF.
**** CS2022000324


    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'DESC_ABSOLUTO'.
      READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.

      MOVE: wg_header-doc_simulacao  TO wa_itens_id-doc,
            wl_itens-vbeln           TO wa_itens_id-vbeln,
            wl_itens-posnr           TO wa_itens_id-posnr,
            ls_good-row_id           TO wa_itens_id-index,
            wl_itens-vlrtot          TO wa_itens_id-vlr_ini.

      IF line_exists( it_itens_id[ index = ls_good-row_id ] ).
        MODIFY it_itens_id FROM wa_itens_id INDEX ls_good-row_id TRANSPORTING doc
                                                                              vbeln
                                                                              posnr
                                                                              index.
*                                                                              VLR_INI.
      ELSE.
        APPEND wa_itens_id TO it_itens_id.
      ENDIF.
      CLEAR: wl_itens, wa_itens_id .
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'MATNR'
                                                           OR fieldname = 'ZMENG'
                                                           OR fieldname = 'VENCI'
                                                           OR fieldname = 'ZWERT'
                                                           OR fieldname = 'DTVENC'
                                                           OR fieldname = 'DESCONTO'
                                                           OR fieldname = 'DESC_ABSOLUTO'
                                                           OR fieldname = 'INCO1'.

      IF ls_good-fieldname =  'ZMENG'.
*        PERFORM CALCULA_ITENS.
      ENDIF.

      READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.

      IF ( ls_good-fieldname NE 'MATNR' ).
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'MATNR'
          IMPORTING
            e_value     = wl_matnr.
      ELSE.
        wl_matnr = ls_good-value.
      ENDIF.

      SELECT SINGLE *
        FROM mara
        INTO wl_mara
       WHERE matnr EQ wl_matnr.

      CLEAR wl_zsdt0087.

      IF ( sy-subrc IS INITIAL ).
        SELECT SINGLE *
          FROM makt
          INTO wl_makt
         WHERE matnr EQ wl_mara-matnr
           AND spras EQ sy-langu.

        SELECT SINGLE *
          FROM kna1
          INTO wl_kna1
         WHERE kunnr EQ wg_header-kunnr.

        SELECT *
          FROM zsdt0042
          INTO TABLE tl_0042
         WHERE cultura EQ wg_header-cultura
           AND waerk   EQ wg_header-waerk
           AND estado  EQ wl_kna1-regio
           AND safra   EQ wg_header-safra  " RJF - 61516 - 2023.08.31
           AND val_de  LE wg_header-dtent  " RJF - 61516 - 2023.08.31
           AND val_ate GE wg_header-dtent. " RJF - 61516 - 2023.08.31

        LOOP AT tl_0042 INTO wl_0042.
          IF ( wl_0042-witht EQ 'FR' ).
            IF wl_kna1-stkzn IS NOT INITIAL.
              IF wg_header-funrural IS INITIAL.
                ADD wl_0042-vlr_perc TO wl_vlr_perc.
              ELSE.
                ADD wl_0042-vlr_perc1 TO wl_vlr_perc.
              ENDIF.
              ADD wl_0042-vlr_aliq TO wl_vlr_aliq.
            ENDIF.
*#138092 - ITSOUZA - Inicio
          ELSEIF ( wl_0042-witht EQ 'FI' ).
            CHECK wl_0042-estado EQ 'GO'.
            IF wg_header-fundeinfra_exce IS INITIAL.
              ADD wl_0042-vlr_perc TO wl_vlr_perc.
            ELSE.
              ADD wl_0042-vlr_perc1 TO wl_vlr_perc.
            ENDIF.
            ADD wl_0042-vlr_aliq TO wl_vlr_aliq.
*#138092 - ITSOUZA - Fim
          ELSE.
            ADD wl_0042-vlr_perc TO wl_vlr_perc.
            ADD wl_0042-vlr_aliq TO wl_vlr_aliq.
          ENDIF.
        ENDLOOP.

        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'ZIEME'
          IMPORTING
            e_value     = wl_zieme.

        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'INCO1'
          IMPORTING
            e_value     = wl_inco1.

        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'WERKS'
          IMPORTING
            e_value     = wl_werks.

        SELECT SINGLE *
          FROM zsdt0036
          INTO wl_0036
         WHERE val_de       LE sy-datum
           AND val_ate      GE sy-datum
           AND matnr        EQ wl_mara-matnr
           AND waerk        EQ wg_header-waerk
           AND meins        EQ wl_zieme
           AND inco1        EQ wl_inco1
           AND safra        EQ wg_header-safra
           AND cultura      EQ wg_header-cultura
           AND loekz        EQ space
           AND werks_fornec EQ wl_itens-werks
           AND eliminado    EQ space
           AND bukrs        EQ wa_t001k-bukrs.

        IF ( ls_good-fieldname EQ 'MATNR' ).
          MOVE wl_0036-dtvenc TO wl_itens-dtvenc.
        ENDIF.

        IF ( ls_good-fieldname NE 'WERKS' ).
          CLEAR wl_inco1.
          CALL METHOD er_data_changed->get_cell_value
            EXPORTING
              i_row_id    = ls_good-row_id
              i_tabix     = ls_good-tabix
              i_fieldname = 'INCO1'
            IMPORTING
              e_value     = wl_inco1.
        ELSE.
          wl_inco1 = ls_good-value.
        ENDIF.

        CLEAR wl_werks.
        IF ( wl_inco1 EQ 'CIF' ).
          IF ls_good-fieldname NE 'WERKS'.
            CALL METHOD er_data_changed->get_cell_value
              EXPORTING
                i_row_id    = ls_good-row_id
                i_tabix     = ls_good-tabix
                i_fieldname = 'WERKS'
              IMPORTING
                e_value     = wl_werks.
          ELSE.
            wl_werks = ls_good-value.
          ENDIF.
        ENDIF.

        IF ( sy-subrc IS INITIAL ).
          SELECT SINGLE *
            FROM zsdt0037
            INTO wl_0037
           WHERE val_de          LE sy-datum
             AND val_ate         GE sy-datum
             AND meins           EQ wl_0036-meins
             AND filial_origem   EQ wl_werks
             AND filial_destino  EQ wg_header-vkbur
             AND matkl           EQ wl_mara-matkl
             AND waers           EQ 'BRL'
             AND bukrs           EQ wa_t001k-bukrs
*             AND VLR_FRETE       EQ (
*
*           SELECT MAX( VLR_FRETE ) FROM  ZSDT0037
*             WHERE VAL_DE          LE SY-DATUM
*               AND VAL_ATE         GE SY-DATUM
*               AND MEINS           EQ WL_0036-MEINS
*               AND FILIAL_ORIGEM   EQ WL_WERKS
*               AND FILIAL_DESTINO  EQ WG_HEADER-VKBUR
*               AND MATKL           EQ WL_MARA-MATKL
*               AND WAERS           EQ WG_HEADER-WAERK
*               AND BUKRS           EQ WA_T001K-BUKRS
*              )
             .

          CASE wl_inco1.
            WHEN 'CIF' OR 'CPT' OR 'CFR'.
              PERFORM atualiza_frete CHANGING wl_0037-vlr_frete.
            WHEN OTHERS.
              wl_0037-vlr_frete = 0.
          ENDCASE.

          IF ( ls_good-fieldname NE 'DESCONTO' ).
            CALL METHOD er_data_changed->get_cell_value
              EXPORTING
                i_row_id    = ls_good-row_id
                i_tabix     = ls_good-tabix
                i_fieldname = 'DESCONTO'
              IMPORTING
                e_value     = wl_desconto.
          ELSE.
            wl_desconto = ls_good-value.
          ENDIF.

          IF ( ls_good-fieldname NE 'ZWERT' ).
            CALL METHOD er_data_changed->get_cell_value
              EXPORTING
                i_row_id    = ls_good-row_id
                i_tabix     = ls_good-tabix
                i_fieldname = 'ZWERT'
              IMPORTING
                e_value     = wl_negociado.
          ELSE.
            wl_negociado = ls_good-value.
          ENDIF.

          DATA: lv_value_aux TYPE f.

          IF NOT wl_0036-vlr_venda IS INITIAL AND
            NOT wl_negociado IS INITIAL.
*            NOT WL_0037-VLR_FRETE IS INITIAL.
            TRY.                                                                                           "BUG 46674 - 28.10.2020
                lv_value_aux  =  ( ( wl_0036-vlr_venda / ( wl_negociado - wl_0037-vlr_frete ) ) - 1 ) * 100.
              CATCH cx_sy_zerodivide.
            ENDTRY.

            CLEAR wl_negociado.

          ENDIF.

* Calcula a porcentagem do valor negociado.
*         _Inicio_

*          CLEAR: WL_NEGOCIADO.

          IF ( ls_good-fieldname NE 'ZWERT' ).

*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*              IMPORTING
*                E_VALUE     = WL_NEGOCIADO.

*            CLEAR: WL_CALCULO.

            IF ( wl_desconto IS INITIAL ).
              wl_negociado = wl_itens-vl_unit.

            ELSEIF ( wl_desconto LE 100 ).
*              WL_CALC_AUX =  WL_DESCONTO / 100.
              wl_calc_aux =  lv_value_aux / 100.
              ADD 1 TO wl_calc_aux.
              wl_negociado = ( wl_0036-vlr_venda  / wl_calc_aux ). "( 1 +  wl_desconto / 100 ) ) ).
              ADD wl_0037-vlr_frete TO wl_negociado.

            ELSEIF ( wl_desconto GE 0 ).
              MESSAGE 'Porcentagem de desconto ultrapassa a margem permitida.' TYPE 'S' DISPLAY LIKE 'E'.
              wl_negociado = wl_itens-vl_unit.
              wl_desconto  = space.
            ENDIF.

*            MOVE: WL_NEGOCIADO TO WL_ITENS-ZWERT,
*            MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING ZWERT.

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*                I_VALUE     = LV_VALUE.

*            C_DESC = 'X'.
          ELSE.
*            IF ( C_DESC IS INITIAL ).
*              C_DESC = 'X'.

*            WL_NEGOCIADO = LS_GOOD-VALUE / 10000.

            wl_negociado = ls_good-value.

            IF ( wl_negociado EQ wl_itens-vl_unit ).
              wl_desconto = space.
            ELSE.

              DATA: "LV_VALUE_AUX TYPE F,
                    lv_value     TYPE zsdt0041-desconto.

              IF ( wl_negociado IS NOT INITIAL ).
                "LV_VALUE_AUX  = ( ( WL_0036-VLR_VENDA - ( WL_NEGOCIADO - WL_0037-VLR_FRETE ) ) / ( WL_NEGOCIADO - WL_0037-VLR_FRETE ) ) * 100.
                TRY.                                                                                           "BUG 46674 - 28.10.2020
                    lv_value_aux  =  ( ( wl_0036-vlr_venda / ( wl_negociado - wl_0037-vlr_frete ) ) - 1 ) * 100.
                  CATCH cx_sy_zerodivide.
                ENDTRY.
              ELSE.
                wl_negociado = wl_itens-vl_unit.
                CLEAR lv_value_aux.
              ENDIF.

              IF ( lv_value_aux > 100 ).
                MESSAGE 'Valor negociado ultrapassa a margem permitida p/ desconto.' TYPE 'S' DISPLAY LIKE 'E'.
                wl_negociado = wl_itens-vl_unit.
                wl_desconto  = space.
              ELSE.
                CLEAR : lv_value.
                CALL FUNCTION 'ROUND'
                  EXPORTING
                    decimals = 1
                    input    = lv_value_aux
                  IMPORTING
                    output   = lv_value.

                wl_desconto  = lv_value.
              ENDIF.
            ENDIF.

*               WL_NEGOCIADO = WL_NEGOCIADO - WL_0037-VLR_FRETE.
*               WL_DESCONTO = WL_0036-VLR_VENDA - WL_NEGOCIADO .
*               WL_DESCONTO = WL_DESCONTO /  WL_NEGOCIADO.
*               WL_DESCONTO = WL_DESCONTO * 100.

*            MOVE: WL_DESCONTO  TO WL_ITENS-DESCONTO,
*                  WL_NEGOCIADO TO WL_ITENS-ZWERT.
*
*            MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING DESCONTO
*                                                                            ZWERT.

*              CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*                EXPORTING
*                  I_ROW_ID    = LS_GOOD-ROW_ID
*                  I_TABIX     = LS_GOOD-TABIX
*                  I_FIELDNAME = 'DESCONTO'
*                  I_VALUE     = LV_VALUE.
*            ENDIF.

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*                I_VALUE     = LS_GOOD-VALUE.
          ENDIF.

          MOVE: wl_desconto  TO wl_itens-desconto,
                wl_negociado TO wl_itens-zwert.

          MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING desconto
                                                                          zwert.

*         _fim _


*** Realiza o calculo do campo "Margem Efetiva"

*          CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'ZWERT'
*            IMPORTING
*              E_VALUE     = WL_PRUNIT.

          TRY.
              wl_mgefe = ( ( ( wl_negociado - wl_0036-vlr_custo - wl_0037-vlr_frete ) / wl_negociado ) * 100 ).
*                move: wl_mgefe_aux to wl_mgefe.
            CATCH:
              cx_sy_zerodivide, cx_sy_arithmetic_overflow.
          ENDTRY.

          MOVE wl_mgefe TO wl_itens-mgefe.
          MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING mgefe.
*           ____


*** Realiza o calculo do campo "Valor Total item"

*          CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'ZWERT'
*            IMPORTING
*              E_VALUE     = WL_PRUNIT.

          CALL METHOD er_data_changed->get_cell_value
            EXPORTING
              i_row_id    = ls_good-row_id
              i_tabix     = ls_good-tabix
              i_fieldname = 'DESC_ABSOLUTO'
            IMPORTING
              e_value     = wl_desc_abs.

          CALL METHOD er_data_changed->get_cell_value
            EXPORTING
              i_row_id    = ls_good-row_id
              i_tabix     = ls_good-tabix
              i_fieldname = 'ZMENG'
            IMPORTING
              e_value     = wl_zmeng.

          wl_vlrtot = wl_negociado * wl_zmeng - wl_desc_abs.

          MOVE wl_vlrtot TO wl_itens-vlrtot.

          MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING vlrtot.
*           ____

          IF ( ls_good-fieldname EQ 'DESCONTO' ).
            wl_desconto = ls_good-value.
          ENDIF.

          IF wg_header-tpsim(1) EQ c_tro(1).

*            CLEAR: WL_CALCULO.
*
*            WL_CALC_AUX =  WL_DESCONTO / 100.
*            ADD 1 TO WL_CALC_AUX.
*            WL_CALCULO = ( WL_0036-VLR_VENDA  / WL_CALC_AUX ). "( 1 +  wl_desconto / 100 ) ) ).
*            ADD WL_0037-VLR_FRETE TO  WL_CALCULO.
*
*            LV_VALUE =  WL_CALCULO.

            MOVE wl_negociado TO wl_itens-calcu.
            MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING calcu.

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'CALCU'
*                I_VALUE     = LV_VALUE.

*            CLEAR: WL_CALCULO, WL_TUNIT.
*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'CALCU'
*              IMPORTING
*                E_VALUE     = WL_CALCULO.

** Realiza o calculo do campo "Troca Unitaria"
*            CLEAR: WL_TOT_VLR, WL_PRUNIT.
            TRY.
*                CLEAR: WL_CALCULO.
*                CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*                  EXPORTING
*                    I_ROW_ID    = LS_GOOD-ROW_ID
*                    I_TABIX     = LS_GOOD-TABIX
*                    I_FIELDNAME = 'CALCU'
*                  IMPORTING
*                    E_VALUE     = WL_CALCULO.

                wl_prunit = wg_header-prec_ant_cult. "/ ( ( WG_HEADER-ANTEC / 100 ) + 1 ).
                wl_tunit  = ( wl_negociado / ( ( wl_prunit - ( ( wl_prunit * ( wl_vlr_perc / 100 ) ) + ( wl_vlr_aliq ) ) ) ) ).
*                wl_tunit = ( wl_calculo / wg_header-prec_cult ) *
*                             ( wl_0039-tx_juros / 100 + 1 ).
              CATCH cx_sy_zerodivide .
            ENDTRY.

            MOVE wl_tunit TO wl_itens-trunit.
            MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING trunit.

*            MOVE: WL_TUNIT TO LV_VALUE.

            CALL METHOD er_data_changed->modify_cell(
                i_row_id    = ls_good-row_id
                i_tabix     = ls_good-tabix
                i_fieldname = 'TRUNIT'
                i_value     = wl_itens-trunit ).

**  Realiza o calculo do campo "Valor Unitario"
*            if wg_header-tpsim(1) eq c_tro(1).
*            wl_prunit = wl_tunit  *  wg_header-prec_cult / 100 + 1.

*            CLEAR: WL_PRUNIT.
            TRY.
*                CLEAR: WL_TUNIT.
*                CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*                  EXPORTING
*                    I_ROW_ID    = LS_GOOD-ROW_ID
*                    I_TABIX     = LS_GOOD-TABIX
*                    I_FIELDNAME = 'TRUNIT'
*                  IMPORTING
*                    E_VALUE     = WL_TUNIT.

                wl_prunit       = wg_header-prec_ant_cult. "/ ( ( WG_HEADER-ANTEC / 100 ) + 1 ).
                wl_tot_vlr      = ( wl_prunit - ( ( wl_prunit * ( wl_vlr_perc / 100 ) ) + ( wl_vlr_aliq ) ) ).
                wl_tot_vlr_unit = wl_tunit * wl_tot_vlr.
*                WL_XB = ( wl_prunit * ( wl_vlr_perc / 100 ) ).
*                WL_XC = ( wl_vlr_aliq ).
*                WL_XD = ( wl_prunit - ( WL_XB + WL_XC ) ).
*                wl_tot_vlr_unit = wl_tunit * WL_XD.

              CATCH cx_sy_zerodivide .
            ENDTRY.
*            CLEAR: WL_PRUNIT.
*            MOVE: WL_TOT_VLR_UNIT TO WL_PRUNIT.

*            endif.
*            MOVE: WL_PRUNIT TO LV_VALUE.

            MOVE wl_tot_vlr_unit TO wl_itens-zwert.
            MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING zwert.

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*                I_VALUE     = LV_VALUE.

** Realiza o calculo do campo "Troca Total Item"
*            CLEAR: WL_TUNIT, WL_PRUNIT.

            CALL METHOD er_data_changed->get_cell_value
              EXPORTING
                i_row_id    = ls_good-row_id
                i_tabix     = ls_good-tabix
                i_fieldname = 'TRUNIT'
              IMPORTING
                e_value     = wl_tunit.

            CALL METHOD er_data_changed->get_cell_value
              EXPORTING
                i_row_id    = ls_good-row_id
                i_tabix     = ls_good-tabix
                i_fieldname = 'ZMENG'
              IMPORTING
                e_value     = wl_zmeng.

            wl_trtot = wl_tunit * wl_zmeng.

            MOVE wl_trtot TO wl_itens-trtot.
            MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING trtot.

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'TRTOT'
*                I_VALUE     = LV_VALUE.

*** Realiza o calculo do campo "Valor Total item"
**            CLEAR: WL_PRUNIT.
*
*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*              IMPORTING
*                E_VALUE     = WL_PRUNIT.
*
*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZMENG'
*              IMPORTING
*                E_VALUE     = WL_ZMENG.
*
*            WL_VLRTOT = WL_PRUNIT * WL_ZMENG.
*
*            MOVE WL_VLRTOT TO WL_ITENS-VLRTOT.
*            MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING VLRTOT.

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'VLRTOT'
*                I_VALUE     = LV_VALUE.

**          _____ Realiza o calculo do campo "Margem Efetiva" ____
*
*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*              IMPORTING
*                E_VALUE     = WL_PRUNIT.
*
*            TRY.
*                WL_MGEFE = ( ( ( WL_PRUNIT - WL_0036-VLR_CUSTO - WL_0037-VLR_FRETE ) / WL_PRUNIT ) * 100 ).
**                move: wl_mgefe_aux to wl_mgefe.
*              CATCH CX_SY_ZERODIVIDE.
*            ENDTRY.
*
*            MOVE WL_MGEFE TO WL_ITENS-MGEFE.
*            MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING MGEFE.
*
**           ____

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'MGEFE'
*                I_VALUE     = LV_VALUE.

          ELSEIF wg_header-tpsim(1) EQ c_adt(1).
**  Realiza o calculo do campo "Valor Unitario"
*            IF WL_DESCONTO GT 0.
*            WL_CALC_AUX =  WL_DESCONTO / 100.
*            ADD 1 TO WL_CALC_AUX.
*            WL_CALCULO = ( WL_0036-VLR_VENDA  / WL_CALC_AUX ). "( 1 +  wl_desconto / 100 ) ) ).
*            ADD WL_0037-VLR_FRETE TO WL_CALCULO.
*            MOVE: WL_CALCULO  TO WL_PRUNIT.
**            ELSE.
**              WL_PRUNIT =  ( WL_0036-VLR_VENDA + WL_0037-VLR_FRETE ).
**            ENDIF.
*            MOVE: WL_PRUNIT TO LV_VALUE.
*
*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_FIELDNAME = 'ZWERT'
*                I_VALUE     = LV_VALUE.

** Realiza o calculo do campo "Valor Total item"
*            CLEAR: WL_PRUNIT, WL_ZMENG.

*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*              IMPORTING
*                E_VALUE     = WL_PRUNIT.
*
*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZMENG'
*              IMPORTING
*                E_VALUE     = WL_ZMENG.
*
*            WL_VLRTOT = WL_PRUNIT * WL_ZMENG.
*            MOVE: WL_VLRTOT TO LV_VALUE.
*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'VLRTOT'
*                I_VALUE     = LV_VALUE.

** Realiza o calculo do campo "Compromisso"
*            CLEAR: WL_VLRTOT, WL_COMPR.

*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'VLRTOT'
*              IMPORTING
*                E_VALUE     = WL_VLRTOT.



            TRY.
                wl_compr = wl_itens-vlrtot / wg_header-vlr_adto.
              CATCH cx_sy_zerodivide.
            ENDTRY.

            MOVE wl_compr TO wl_itens-compr.
            MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING compr.

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'COMPR'
*                I_VALUE     = LV_VALUE.

** Realiza o calculo do campo "Margem Efetiva"

*            CLEAR: WL_PRUNIT, WL_MGEFE_AUX, WL_MGEFE.
*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*              IMPORTING
*                E_VALUE     = WL_PRUNIT.
*            TRY.
*                WL_MGEFE = ( ( ( WL_PRUNIT - WL_0036-VLR_CUSTO - WL_0037-VLR_FRETE ) / WL_PRUNIT ) * 100 ).
**                move: wl_mgefe_aux to wl_mgefe.
*              CATCH CX_SY_ZERODIVIDE.
*            ENDTRY.
*
*            MOVE: WL_MGEFE TO LV_VALUE.
*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'MGEFE'
*                I_VALUE     = LV_VALUE.

          ELSEIF wg_header-tpsim(1) EQ c_vis(1) OR ls_good-fieldname EQ 'DTVENC'.
**          Realiza o calculo do campo "Valor Unitario"
*            BREAK ABAP.
*            IF WL_DESCONTO GT 0.
*            WL_CALC_AUX =  WL_DESCONTO / 100.
*            ADD 1 TO WL_CALC_AUX.
*            WL_CALCULO = ( WL_0036-VLR_VENDA  / WL_CALC_AUX ). "( 1 +  wl_desconto / 100 ) ) ).
*            ADD WL_0037-VLR_FRETE TO WL_NEGOCIADO.
*            MOVE: WL_CALCULO  TO WL_PRUNIT.
*            ELSE.
*              WL_PRUNIT = ( WL_0036-VLR_VENDA + WL_0037-VLR_FRETE ).
*            ENDIF.


*            _____________________________JUROS____________________________________

*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'DTVENC'
*              IMPORTING
*                E_VALUE     = WL_DTVENC.
*
*            WL_DIAS_VCT = 0.
*
*            IF ( WG_HEADER-DTINIJUROS IS NOT INITIAL ).
*              WL_DIAS_VCT = WG_HEADER-DTPGTCULT - WG_HEADER-DTINIJUROS.
*            ENDIF.
*
*            IF ( WL_DIAS_VCT GT 0 ).
*              WL_JURO_DIAS = ( ( WG_HEADER-JUROS_ANO / 365 ) * WL_DIAS_VCT ) / 100.
*              WL_NEGOCIADO = WL_NEGOCIADO + ( WL_NEGOCIADO * WL_JURO_DIAS ).
*            ENDIF.
*
*            MOVE WL_NEGOCIADO TO WL_ITENS-ZWERT.
*            MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING ZWERT.

*           ______________________________________________________________________

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*                I_VALUE     = LV_VALUE.

** Realiza o calculo do campo "Valor Total item"

*            CLEAR: WL_PRUNIT, WL_ZMENG.
*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*              IMPORTING
*                E_VALUE     = WL_PRUNIT.
*
*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZMENG'
*              IMPORTING
*                E_VALUE     = WL_ZMENG.
*
*            WL_VLRTOT = WL_PRUNIT * WL_ZMENG.
*
*            MOVE: WL_VLRTOT TO LV_VALUE.
*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'VLRTOT'
*                I_VALUE     = LV_VALUE.

** Realiza o calculo do campo "Margem Efetiva"
*            CLEAR: WL_PRUNIT, WL_MGEFE_AUX, WL_MGEFE.
*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*              IMPORTING
*                E_VALUE     = WL_PRUNIT.
*
*            TRY.
*                WL_MGEFE = ( ( ( WL_PRUNIT - WL_0036-VLR_CUSTO - WL_0037-VLR_FRETE ) / WL_PRUNIT ) * 100 ).
**                move: wl_mgefe_aux to wl_mgefe.
*              CATCH CX_SY_ZERODIVIDE.
*            ENDTRY.
*            MOVE: WL_MGEFE TO LV_VALUE.
*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'MGEFE'
*                I_VALUE     = LV_VALUE.
          ENDIF.

          MOVE: wl_makt-maktx         TO wl_itens-maktx,
                wl_0036-werks_fornec  TO wl_itens-werks,
                wl_0036-meins         TO wl_itens-zieme,
                wl_0036-inco1         TO wl_itens-inco1,
                wl_0036-perc_margem   TO wl_itens-mgcad.

          MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING maktx
                                                                          werks
                                                                          zieme
                                                                          inco1
                                                                          mgcad
                                                                          dtvenc.

*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'MAKTX'
*              I_VALUE     = LV_VALUE.

*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'WERKS'
*              I_VALUE     = LV_VALUE.

*          MOVE: WL_0036-DTVENC  TO LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_FIELDNAME = 'DTVENC'
*              I_VALUE     = LV_VALUE.

*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'ZIEME'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'INCO1'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'MGCAD'
*              I_VALUE     = LV_VALUE.

*          CLEAR WL_AUART.

          CALL METHOD er_data_changed->get_cell_value
            EXPORTING
              i_row_id    = ls_good-row_id
              i_tabix     = ls_good-tabix
              i_fieldname = 'INCO1'
            IMPORTING
              e_value     = wl_inco1.

          CONDENSE wl_inco1 NO-GAPS.

          "ALRS
          CLEAR wl_zsdt0087.
          SELECT SINGLE *
            FROM zsdt0087
            INTO wl_zsdt0087
            WHERE matkl = wl_mara-matkl
            AND   tpsim = wg_header-tpsim
            AND   inco1 = wl_inco1.

          IF ( sy-subrc = 0 ).

            MOVE: wl_zsdt0087-auart TO wl_itens-auart,
                  wl_zsdt0087-spart TO wl_itens-spart,
                  wl_zsdt0087-auart TO wl_auart.

            MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING auart
                                                                            spart.


*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'AUART'
*                I_VALUE     = VL_VALUE.

* ZDEF e ZODF Removido conforme Chamado CS2016000364
            IF ( "WL_AUART EQ 'ZDEF' OR
                 wl_auart EQ 'ZFTE'
            OR   wl_auart EQ 'ZOFE'
*            OR   WL_AUART EQ 'ZODF'
            OR   wl_auart EQ 'ZFUT' ).

              MOVE wg_header-safra TO wl_itens-charg.


**** Clear no campo Charg US109832-CS2023000286 - SC>>>>>
              CASE wl_itens-auart.
                WHEN 'ZFTE' OR 'ZOFE' OR 'ZFUT'.
                  IF wl_itens-spart EQ '03'.
                    CLEAR: wl_itens-charg.
                  ENDIF.
              ENDCASE.
**** Clear no campo Charg US109832-CS2023000286 - SC>>>>>


**** Clear no campo Charg CS2016001142>>>>>
**** CS2022000324
              CASE wl_itens-auart.
                WHEN 'ZFTE' OR 'ZOFE'.
*                  IF wl_itens-werks EQ '0175' AND wl_mara-mtart EQ 'ZFER'.
                  IF wl_itens-werks IN r_werks AND wl_mara-mtart EQ 'ZFER'.
                    CLEAR wl_itens-charg.
                  ENDIF.
              ENDCASE.
**** CS2022000324
**** Clear no campo Charg CS2016001142<<<<<

              MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING charg.

*              CONDENSE VL_VALUE NO-GAPS.
*              CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*                EXPORTING
*                  I_ROW_ID    = LS_GOOD-ROW_ID
*                  I_TABIX     = LS_GOOD-TABIX
*                  I_FIELDNAME = 'CHARG'
*                  I_VALUE     = VL_VALUE.

            ENDIF.

            MOVE wl_zsdt0087-spart TO wl_itens-spart.
            MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING spart.

*            CONDENSE VL_VALUE NO-GAPS.
*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'SPART'
*                I_VALUE     = VL_VALUE.

          ELSE.

            MOVE: space TO wl_itens-auart,
                  space TO wl_itens-spart.

            MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING auart
                                                                            spart.

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'AUART'
*                I_VALUE     = VL_VALUE.
*            "
*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'SPART'
*                I_VALUE     = VL_VALUE.
          ENDIF.

        ELSE.
          MOVE: space TO wl_itens-calcu,
                space TO wl_itens-trunit,
                space TO wl_itens-trtot,
                space TO wl_itens-compr,
                space TO wl_itens-mgcad,
                space TO wl_itens-zwert,
                space TO wl_itens-vlrtot,
                space TO wl_itens-auart,
                space TO wl_itens-spart,
                space TO wl_itens-maktx,
                space TO wl_itens-werks,
                space TO wl_itens-dtvenc,
                space TO wl_itens-inco1,
                space TO wl_itens-zieme.

          MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING calcu
                                                                          trunit
                                                                          trtot
                                                                          compr
                                                                          mgcad
                                                                          zwert
                                                                          vlrtot
                                                                          auart
                                                                          spart
                                                                          maktx
                                                                          werks
                                                                          dtvenc
                                                                          inco1
                                                                          zieme.

*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'CALCU'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'TRUNIT'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'TRTOT'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'COMPR'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'MGCAD'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'ZWERT'
*              I_VALUE     = LV_VALUE.

*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'VLRTOT'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'AUART'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'SPART'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'MAKTX'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'WERKS'
*              I_VALUE     = LV_VALUE.

*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'DTVENC'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'INCO1'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'ZIEME'
*              I_VALUE     = LV_VALUE.
        ENDIF.
      ELSE.
        MOVE: space TO wl_itens-calcu,
              space TO wl_itens-trunit,
              space TO wl_itens-trtot,
              space TO wl_itens-compr,
              space TO wl_itens-mgcad,
              space TO wl_itens-zwert,
              space TO wl_itens-vlrtot,
              space TO wl_itens-auart,
              space TO wl_itens-spart,
              space TO wl_itens-maktx,
              space TO wl_itens-werks,
              space TO wl_itens-dtvenc,
              space TO wl_itens-inco1,
              space TO wl_itens-zieme.

        MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING calcu
                                                                        trunit
                                                                        trtot
                                                                        compr
                                                                        mgcad
                                                                        zwert
                                                                        vlrtot
                                                                        auart
                                                                        spart
                                                                        maktx
                                                                        werks
                                                                        dtvenc
                                                                        inco1
                                                                        zieme.

*        MOVE: SPACE TO LV_VALUE.
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'CALCU'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'TRUNIT'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'TRTOT'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'COMPR'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'ZWERT'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'VLRTOT'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'AUART'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'SPART'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'MAKTX'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'WERKS'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'DTVENC'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'INCO1'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'MGCAD'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'ZIEME'
*            I_VALUE     = LV_VALUE.
      ENDIF.
      CLEAR: wl_makt, wl_mara, lv_value, wl_0036,
             wl_prunit, wl_calculo, wl_tunit, tg_setleaf, tg_setlinet,
             wl_vlr_perc, wl_vlr_aliq, wl_zieme, wl_inco1, wl_kna1, wl_matnr,
             wl_desconto, wl_itens, wl_calc_aux, wl_negociado, wl_0037, wl_mgefe,
             wl_desc_abs, wl_zmeng, wl_vlrtot, wl_prunit, wl_tunit, wl_tot_vlr,
             wl_tot_vlr_unit, wl_trtot, wl_zsdt0087.

      FREE: tl_0042.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'DESC_ABSOLUTO'.

      READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.
      wa_itens_id = it_itens_id[ index = ls_good-row_id ].

      wa_itens_id-vlr_atu =  wl_itens-vlrtot.
      wa_itens_id-vlr_dif = wa_itens_id-vlr_atu - wa_itens_id-vlr_ini.

      MODIFY it_itens_id
        FROM wa_itens_id
          TRANSPORTING vlr_atu
                       vlr_dif
                       WHERE index EQ ls_good-row_id .

    ENDLOOP.


    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'ZWERT' OR fieldname = 'MATNR'.
      READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.
      PERFORM busca_imposto CHANGING wl_itens.
      MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING vlr_icms zwert_liqdo.
    ENDLOOP.

    PERFORM verifica_erros.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen    = '100'
        i_show      = space
        i_repid     = sy-repid
        i_popup     = 0
*       i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
        i_set_field = 'X_FIELD'
      IMPORTING
        e_messagem  = wg_mensagem
      TABLES
        it_msgs     = tg_msg_ret.
  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD on_data_changed_finished.

    TYPES: BEGIN OF tyl_mara,
             matnr TYPE mara-matnr,
             matkl TYPE setleaf-valfrom,
           END OF tyl_mara.

    DATA: wl_itens        LIKE LINE OF tg_itens,
          tl_mara         TYPE TABLE OF tyl_mara,
          wl_mara         TYPE tyl_mara,
          wl_mara_aux     TYPE mara,
          tl_makt         TYPE TABLE OF makt,
          wl_makt         TYPE makt,
          tl_0036         TYPE TABLE OF zsdt0036,
          wl_0036         TYPE zsdt0036,
          tl_0042         TYPE TABLE OF zsdt0042,
          wl_0042         TYPE zsdt0042,
          tl_0037         TYPE TABLE OF zsdt0037,
          wl_0037         TYPE zsdt0037,
          wl_0039         TYPE zsdt0039,
          wl_vlr_perc     TYPE zsdt0042-vlr_perc,
          wl_vlr_aliq     TYPE zsdt0042-vlr_aliq,
          wl_prunit       TYPE zsdt0041-zwert,
          wl_mgefe_aux    TYPE zsdt0041-zwert,
          wl_tot_vlr      TYPE zsdt0041-zwert,
          wl_tot_vlr_unit TYPE zsdt0041-zwert,
          ls_good         TYPE lvc_s_modi,
          tl_setleaf      TYPE TABLE OF setleaf,
          wl_setleaf      TYPE setleaf,
          tl_setlinet     TYPE TABLE OF setlinet,
          wl_setlinet     TYPE setlinet,
          wl_desconto     TYPE zsdt0041-desconto,
          wl_calc_aux     TYPE zsdt0041-calcu,
          wl_kna1         TYPE kna1,
          wl_trototsc(30),
          wl_tabix        TYPE sy-tabix,
          wl_valor_aux    LIKE wg_header-trototsc,
          wl_zsdt0087     TYPE zsdt0087,
          tl_itens        LIKE TABLE OF tg_itens.




    CLEAR: wl_itens, wl_mara, wl_makt, wl_0036, wl_0042, wl_0037, wl_0039, wl_vlr_perc,
           wl_vlr_aliq, wl_prunit, wl_tot_vlr, wl_tot_vlr_unit, ls_good, tg_setleaf, tg_setlinet,
           tl_0037, wl_desconto, wl_mgefe_aux, wl_calc_aux, wl_kna1, wl_valor_aux.

*    REFRESH: TL_MARA, TL_MAKT, TL_0036, TL_0042, TL_ITENS.

    CHECK et_good_cells IS NOT INITIAL.

*    SELECT MATNR MATKL
*      FROM MARA
*      INTO TABLE TL_MARA
*   FOR ALL ENTRIES IN TG_ITENS
*     WHERE MATNR EQ TG_ITENS-MATNR.
*
*      IF SY-SUBRC IS INITIAL.
*        SELECT *
*         FROM MAKT
*         INTO TABLE TL_MAKT
*          FOR ALL ENTRIES IN TL_MARA
*          WHERE MATNR EQ TL_MARA-MATNR.
*
*        SELECT *
*           FROM ZSDT0036
*           INTO TABLE TL_0036
*           FOR ALL ENTRIES IN TL_MARA
*            WHERE VAL_DE    LE SY-DATUM
*              AND VAL_ATE   GE SY-DATUM
*              AND MATNR     EQ TL_MARA-MATNR
*              AND WAERK     EQ WG_HEADER-WAERK
*              AND SAFRA     EQ WG_HEADER-SAFRA
*              AND LOEKZ     EQ SPACE
*              AND CULTURA   EQ WG_HEADER-CULTURA
*              AND ELIMINADO EQ SPACE.
**              and werks   eq wg_header-vkbur.
*      ENDIF.
*
*
*      SELECT SINGLE *
*        FROM KNA1
*        INTO WL_KNA1
*         WHERE KUNNR EQ WG_HEADER-KUNNR.
*
*      SELECT *
*          FROM ZSDT0042
*          INTO TABLE TL_0042
*           WHERE CULTURA EQ WG_HEADER-CULTURA
*             AND WAERK   EQ WG_HEADER-WAERK
*             AND ESTADO  EQ WL_KNA1-REGIO.
*
*
*
*      CLEAR: WL_VLR_PERC, WL_VLR_ALIQ.
*      LOOP AT TL_0042 INTO WL_0042.
*
*        IF WL_0042-WITHT EQ 'FR'.
*          IF WL_KNA1-STKZN IS NOT INITIAL.
*            ADD WL_0042-VLR_PERC TO WL_VLR_PERC.
*            ADD WL_0042-VLR_ALIQ TO WL_VLR_ALIQ.
*          ENDIF.
*        ELSE.
*          ADD WL_0042-VLR_PERC TO WL_VLR_PERC.
*          ADD WL_0042-VLR_ALIQ TO WL_VLR_ALIQ.
*        ENDIF.
*
*      ENDLOOP.
*
*      SELECT *
*        FROM ZSDT0037
*        INTO TABLE TL_0037
*         FOR ALL ENTRIES IN TG_ITENS
*         WHERE VAL_DE          LE SY-DATUM
*           AND VAL_ATE         GE SY-DATUM
*           AND MEINS           EQ TG_ITENS-ZIEME
*           AND FILIAL_ORIGEM   EQ TG_ITENS-WERKS
*           AND FILIAL_DESTINO  EQ WG_HEADER-VKBUR
*           AND WAERS           EQ WG_HEADER-WAERK.
*
*    ENDIF.

    LOOP AT et_good_cells INTO ls_good.
      READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.
*
**      READ TABLE TL_MARA INTO WL_MARA
**        WITH KEY MATNR = WL_ITENS-MATNR.
*
      SELECT SINGLE *
        FROM mara
        INTO wl_mara_aux
        WHERE matnr = wl_itens-matnr.

      IF ( sy-subrc IS INITIAL ).

        SELECT SINGLE *
          FROM zsdt0087
          INTO wl_zsdt0087
         WHERE matkl = wl_mara_aux-matkl
           AND tpsim = wg_header-tpsim
           AND inco1 = wl_itens-inco1.

        IF ( sy-subrc IS INITIAL ).
          MOVE: wl_zsdt0087-auart TO wl_itens-auart,
                wl_zsdt0087-spart TO wl_itens-spart.
        ENDIF.
      ENDIF.
*

**     ___________________________________Commented by Enio Jesus___________________________________
*

*        READ TABLE TL_MAKT INTO WL_MAKT
*          WITH KEY MATNR = WL_MARA-MATNR.
*
*        MOVE WL_MAKT-MAKTX TO WL_ITENS-MAKTX.
*
*        READ TABLE TL_0036 INTO WL_0036
*                  WITH KEY MATNR = WL_MARA-MATNR
*                           INCO1 = WL_ITENS-INCO1
*                           MEINS = WL_ITENS-ZIEME
*                           SAFRA = WG_HEADER-SAFRA
*                           WERKS_FORNEC = WL_ITENS-WERKS
*                           DTVENC = WL_ITENS-DTVENC.

*        IF ( SY-SUBRC IS INITIAL ).
*          IF ( WL_ITENS-INCO1 EQ 'CIF' ).
*            CLEAR: WL_0037.
*            READ TABLE TL_0037 INTO WL_0037
*              WITH KEY FILIAL_ORIGEM  = WL_ITENS-WERKS
*                       MEINS          = WL_ITENS-ZIEME
*                       FILIAL_DESTINO = WG_HEADER-VKBUR
*                       WAERS          = WG_HEADER-WAERK
*                       MATKL          = WL_MARA-MATKL.
*          ELSE.
*            CLEAR: WL_0037.
*          ENDIF.
*
*          MOVE: WL_0036-MEINS        TO WL_ITENS-ZIEME,
*                WL_0036-PERC_MARGEM  TO WL_ITENS-MGCAD,
*                WL_0036-WERKS_FORNEC TO WL_ITENS-WERKS.
*
*          IF ( LS_GOOD-FIELDNAME EQ 'MATNR' ).
*            MOVE WL_0036-DTVENC TO WL_ITENS-DTVENC.
*          ENDIF.
*
*          IF WG_HEADER-TPSIM(1) EQ C_TRO(1).
*            CLEAR: WL_ITENS-CALCU, WL_PRUNIT, WL_ITENS-TRUNIT, WL_ITENS-ZWERT, WL_ITENS-TRTOT,
*                   WL_ITENS-MGEFE, WL_MGEFE_AUX, WL_CALC_AUX.
***            Realiza o calculo do campo "Calculo"
**            IF WL_ITENS-DESCONTO GT 0.
*
**            WL_CALC_AUX =  WL_ITENS-DESCONTO / 100.
**            ADD 1 TO WL_CALC_AUX.
**            WL_ITENS-CALCU = ( WL_0036-VLR_VENDA / WL_CALC_AUX ).
**            ADD WL_0037-VLR_FRETE  TO WL_ITENS-CALCU.
*
*
**              MOVE: WL_ITENS-CALCU TO WL_ITENS-ZWERT.
**            ELSE.
**              WL_ITENS-CALCU = ( WL_0036-VLR_VENDA + WL_0037-VLR_FRETE ).
**              WL_ITENS-ZWERT = WL_ITENS-CALCU.
**            ENDIF.
*
***            Realiza o calculo do campo "Troca Unitaria"
**            TRY.
**                WL_PRUNIT = WG_HEADER-PREC_CULT / ( ( WG_HEADER-ANTEC / 100 ) + 1 ).
**                WL_ITENS-TRUNIT = ( WL_ITENS-CALCU / ( ( WL_PRUNIT - ( ( WL_PRUNIT * ( WL_VLR_PERC / 100 ) ) + ( WL_VLR_ALIQ ) ) ) ) ).
**              CATCH CX_SY_ZERODIVIDE.
**            ENDTRY.
*
***            Realiza o calculo do campo "Valor Unitario"
*            CLEAR: WL_PRUNIT.
*            TRY.
*                WL_PRUNIT = WG_HEADER-PREC_CULT / ( ( WG_HEADER-ANTEC / 100 ) + 1 ).
*                WL_TOT_VLR = ( WL_PRUNIT - ( ( WL_PRUNIT * ( WL_VLR_PERC / 100 ) ) + ( WL_VLR_ALIQ ) ) ).
*                WL_ITENS-ZWERT = WL_ITENS-TRUNIT * WL_TOT_VLR.
**                MOVE WL_ITENS-ZWERT TO WL_ITENS-VL_UNIT.
*              CATCH CX_SY_ZERODIVIDE.
*            ENDTRY.
***            Realiza o calculo do campo "Troca Total Item"
*            WL_ITENS-TRTOT = WL_ITENS-TRUNIT * WL_ITENS-ZMENG.
***            Realiza o calculo do campo "valor Total Item"
*            WL_ITENS-VLRTOT = ( WL_ITENS-ZWERT * WL_ITENS-ZMENG ) - WL_ITENS-DESC_ABSOLUTO.
***           Realiza o calculo do campo "Margem Efetiva"
*            TRY.
*                WL_ITENS-MGEFE = ( ( ( WL_ITENS-ZWERT - WL_0036-VLR_CUSTO - WL_0037-VLR_FRETE ) / WL_ITENS-ZWERT ) * 100 ).
**                move: wl_mgefe_aux to wl_itens-mgefe.
*              CATCH CX_SY_ZERODIVIDE.
*                WL_ITENS-MGEFE = 0.
*            ENDTRY.
*
*          ELSEIF WG_HEADER-TPSIM(1) EQ C_ADT(1).
*            CLEAR: WL_ITENS-ZWERT, WL_ITENS-VLRTOT, WL_ITENS-COMPR, WL_ITENS-MGEFE, WL_MGEFE_AUX,
*                   WL_CALC_AUX.
***          Realiza o calculo do campo "Valor Unitario"
**            IF WL_ITENS-DESCONTO GT 0.
*
*
**            WL_CALC_AUX =  WL_ITENS-DESCONTO / 100.
**            ADD 1 TO WL_CALC_AUX.
**            WL_ITENS-ZWERT = ( WL_0036-VLR_VENDA / WL_CALC_AUX ).
***              MOVE WL_ITENS-ZWERT TO WL_ITENS-VL_UNIT.
**            ADD WL_0037-VLR_FRETE  TO WL_ITENS-ZWERT.
**
***            ELSE.
***              WL_ITENS-VL_UNIT = WL_ITENS-ZWERT =  ( WL_0036-VLR_VENDA + WL_0037-VLR_FRETE ).
***              MOVE WL_ITENS-ZWERT TO WL_ITENS-VL_UNIT.
***            ENDIF.
****          Realiza o calculo do campo "Valor Total item"
**            WL_ITENS-VLRTOT = ( WL_ITENS-ZWERT * WL_ITENS-ZMENG ) - WL_ITENS-DESC_ABSOLUTO.
****          Realiza o calculo do campo "Compromisso"
**            TRY.
**                WL_ITENS-COMPR = WL_ITENS-VLRTOT / WG_HEADER-VLR_ADTO.
**              CATCH CX_SY_ZERODIVIDE.
**            ENDTRY.
****           Realiza o calculo do campo "Margem Efetiva"
**            TRY.
**                WL_ITENS-MGEFE = ( ( ( WL_ITENS-ZWERT - WL_0036-VLR_CUSTO - WL_0037-VLR_FRETE ) / WL_ITENS-ZWERT ) * 100 ).
***                move: wl_mgefe_aux to wl_itens-mgefe.
**              CATCH CX_SY_ZERODIVIDE.
**                WL_ITENS-MGEFE = 0.
**            ENDTRY.
*
*          ELSEIF WG_HEADER-TPSIM(1) EQ C_VIS(1) OR LS_GOOD-FIELDNAME EQ  'DTVENC'.
*            CLEAR: WL_ITENS-ZWERT, WL_ITENS-VLRTOT, WL_ITENS-MGEFE, WL_MGEFE_AUX, WL_CALC_AUX.
***          Realiza o calculo do campo "Valor Unitario"
**            IF WL_ITENS-DESCONTO GT 0.
*            WL_CALC_AUX =  WL_ITENS-DESCONTO / 100.
*            ADD 1 TO WL_CALC_AUX.
*            WL_ITENS-ZWERT = ( WL_0036-VLR_VENDA / WL_CALC_AUX ).
**              MOVE WL_ITENS-ZWERT TO WL_ITENS-VL_UNIT.
*            ADD WL_0037-VLR_FRETE  TO WL_ITENS-ZWERT.
**              WL_ITENS-ZWERT = WL_ITENS-CALCU.
**            ELSE.
**              WL_ITENS-ZWERT = ( WL_0036-VLR_VENDA + WL_0037-VLR_FRETE ).
**              MOVE WL_ITENS-ZWERT TO WL_ITENS-VL_UNIT.
**               WL_ITENS-calcu =  WL_ITENS-ZWERT.
**            ENDIF.
*
*            "JUROS
*            WL_DIAS_VCT = 0.
*            IF WG_HEADER-DTINIJUROS IS NOT INITIAL.
*              WL_DIAS_VCT = WG_HEADER-DTPGTCULT - WG_HEADER-DTINIJUROS.
*            ENDIF.
*            IF WL_DIAS_VCT GT 0.
*              WL_JURO_DIAS = ( ( WG_HEADER-JUROS_ANO / 365 ) * WL_DIAS_VCT ) / 100.
*              WL_ITENS-ZWERT = WL_ITENS-ZWERT + ( WL_ITENS-ZWERT * WL_JURO_DIAS ).
**              MOVE WL_ITENS-ZWERT TO WL_ITENS-VL_UNIT.
*            ENDIF.
*
***          Realiza o calculo do campo "Valor Total item"
*            WL_ITENS-VLRTOT = ( WL_ITENS-ZWERT * WL_ITENS-ZMENG ) - WL_ITENS-DESC_ABSOLUTO.
***           Realiza o calculo do campo "Margem Efetiva"
*            TRY.
*                WL_ITENS-MGEFE = ( ( ( WL_ITENS-ZWERT - WL_0036-VLR_CUSTO - WL_0037-VLR_FRETE ) / WL_ITENS-ZWERT ) * 100 ).
**                move: wl_mgefe_aux to wl_itens-mgefe.
*              CATCH CX_SY_ZERODIVIDE.
*                WL_ITENS-MGEFE = 0.
*            ENDTRY.
*          ENDIF.
*        ENDIF.
*      ELSE.
*        CLEAR: WL_ITENS-AUART, WL_ITENS-SPART, WL_ITENS-MAKTX.
*      ENDIF.

**       ___________________________________________________________________________________________

      CASE ls_good-fieldname.
        WHEN 'ZWERT'.
          IF ( wl_itens-desconto IS INITIAL ).
            wl_itens-zwert    = wl_itens-vl_unit.
          ELSE.
            "WL_ITENS-ZWERT = ( WL_ITENS-ZWERT / 10000 ).
          ENDIF.

        WHEN 'DESCONTO'.
          IF ( ls_good-value > 100 ).
            wl_itens-desconto = space.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.

      MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING zwert
                                                                      desconto
                                                                      auart
                                                                      spart.
      CLEAR: wl_makt,
             wl_mara,
             wl_0036,
             wl_itens,
             wl_0037.
    ENDLOOP.

***** Método de atualização de dados na Tela
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
*   PERFORM calcula_header USING wl_tot_vlr.
    IF NOT wg_save EQ c_altcsa.
      PERFORM calcula_header USING wl_tot_vlr.
    ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

*    IF ( WG_HEADER-TPSIM(1) EQ C_ADT(1)
*    OR   WG_HEADER-TPSIM(1) EQ C_B     ).
*
*      CLEAR: WL_TOT_VLR, WG_HEADER-VLRTOT, WG_HEADER-AREA_PENHOR, WG_HEADER-COMPRSC.
*
*      LOOP AT TG_ITENS INTO WL_ITENS.
*        ADD WL_ITENS-VLRTOT TO WL_TOT_VLR.
***    Realiza o calculo do campo "Valor Total"
*        ADD WL_ITENS-VLRTOT TO WG_HEADER-VLRTOT.
***    Realiza o calculo do campo "Compromisso/sac"
*        ADD WL_ITENS-COMPR  TO WG_HEADER-COMPRSC.
*      ENDLOOP.
***    Realiza o calculo do campo "Area de Penhor"
*      TRY.
*          WG_HEADER-AREA_PENHOR = WL_TOT_VLR / WG_HEADER-ADTO_HA.
*        CATCH CX_SY_ZERODIVIDE.
*      ENDTRY.
*
*
*    ELSEIF ( WG_HEADER-TPSIM(1) EQ C_TRO(1) ).
**        BREAK ABAP.
*
***__________________________________________________________________________________
*      CLEAR: WG_HEADER-TROTOTSC, WG_HEADER-SCHA, WG_HEADER-VLRTOT, CONVERT_TRATOTSC.
*      LOOP AT TG_ITENS INTO WL_ITENS.
*
**    Realiza o calculo do campo "Troca Total Sc"
*        ADD: WL_ITENS-TRTOT TO WG_HEADER-TROTOTSC,
*             WL_ITENS-TRTOT TO CONVERT_TRATOTSC.
*
***    Realiza o calculo do campo "Valor Total"
*        ADD WL_ITENS-VLRTOT TO WG_HEADER-VLRTOT.
*
*      ENDLOOP.
*
**      MODIFY TG_ITENS FROM WL_ITENS TRANSPORTING TRTOT VLRTOT.
***__________________________________________________________________________________
*
**        WRITE WG_HEADER-TROTOTSC TO WL_TROTOTSC.
**        CONDENSE WL_TROTOTSC NO-GAPS.
**        FIND ','  IN WL_TROTOTSC MATCH OFFSET WL_TABIX.
***      WL_TABIX = SY-TABIX.
**        ADD 1 TO WL_TABIX.
**        IF WL_TROTOTSC+WL_TABIX(1) GT 5.
**          WL_TROTOTSC+WL_TABIX(3) = '000'.
**          TRANSLATE WL_TROTOTSC USING: '. '.
**          TRANSLATE WL_TROTOTSC USING: ', '.
**          TRANSLATE WL_TROTOTSC+WL_TABIX USING: '0 '.
**          CONDENSE WL_TROTOTSC NO-GAPS.
**
**          MOVE: WL_TROTOTSC TO WG_HEADER-TROTOTSC.
**          ADD 1 TO WG_HEADER-TROTOTSC.
**        ELSEIF WL_TROTOTSC+WL_TABIX(1) LT 5.
**          WL_TROTOTSC+WL_TABIX(3) = '000'.
**          TRANSLATE WL_TROTOTSC USING: '. '.
**          TRANSLATE WL_TROTOTSC USING: ', '.
**          TRANSLATE WL_TROTOTSC+WL_TABIX USING: '0 '.
**          CONDENSE WL_TROTOTSC NO-GAPS.
**
**          MOVE: WL_TROTOTSC TO WG_HEADER-TROTOTSC.
**
**        ENDIF.
***__________________________________________________________________________________
*
*      PERFORM MONTA_MEMO.
***__________________________________________________________________________________
**      WL_VALOR_AUX = WG_HEADER-TROTOTSC * WG_MEMO-PLIQDO_AUX.
**      SUBTRACT WG_HEADER-VLRTOT FROM WL_VALOR_AUX.
**      TL_ITENS[] = TG_ITENS[].
**      SORT: TL_ITENS BY ZMENG DESCENDING.
**      READ TABLE TL_ITENS INTO TG_ITENS INDEX 1.
**      TG_ITENS-ZWERT = ( ( WL_VALOR_AUX / TG_ITENS-ZMENG ) + TG_ITENS-ZWERT ).
**      TG_ITENS-VLRTOT = ( TG_ITENS-ZWERT * TG_ITENS-ZMENG ) - WL_ITENS-DESC_ABSOLUTO.
**      CLEAR: WL_0036.
**      READ TABLE TL_0036 INTO WL_0036
**             WITH KEY MATNR = TG_ITENS-MATNR.
***                   werks = wg_header-vkbur.
**
**      IF TG_ITENS-INCO1 EQ 'CIF'.
**        CLEAR: WL_0037.
**        READ TABLE TL_MARA INTO WL_MARA
**                  WITH KEY MATNR = TG_ITENS-MATNR.
**        READ TABLE TL_0037 INTO WL_0037
**          WITH KEY FILIAL_ORIGEM = TG_ITENS-WERKS
**                   MEINS         = TG_ITENS-ZIEME
**                   MATKL         = WL_MARA-MATKL.\
**      ELSE.
**        CLEAR: WL_0037.
**      ENDIF.
***           Realiza o calculo do campo "Margem Efetiva"
**      TRY.
**          TG_ITENS-MGEFE = ( ( ( TG_ITENS-ZWERT - WL_0036-VLR_CUSTO - WL_0037-VLR_FRETE ) / TG_ITENS-ZWERT ) * 100 ).
***                move: wl_mgefe_aux to wl_itens-mgefe.
**        CATCH CX_SY_ZERODIVIDE.
**          TG_ITENS-MGEFE = 0.
**      ENDTRY.
*
*      CLEAR: WG_HEADER-VLRTOT.
*      LOOP AT TG_ITENS INTO WL_ITENS.
***    Realiza o calculo do campo "Valor Total"
*        ADD WL_ITENS-VLRTOT TO WG_HEADER-VLRTOT.
*      ENDLOOP.
*
****   Calculo Sc/Há
*      TRY.
*          WG_HEADER-SCHA = WG_HEADER-TROTOTSC / WG_AREA_HA. "WG_HEADER-AREA_HA.
*        CATCH CX_SY_ZERODIVIDE.
*      ENDTRY.
*    ELSE.
**    Realiza o calculo do campo "Valor Total"
*      CLEAR: WG_HEADER-VLRTOT.
*      LOOP AT TG_ITENS INTO WL_ITENS.
*        ADD WL_ITENS-VLRTOT TO WG_HEADER-VLRTOT.
*      ENDLOOP.
*
*    ENDIF.

    IF ( e_modified IS NOT INITIAL ).
      LEAVE TO SCREEN 100.
    ENDIF.

*    REFRESH: TL_MARA, TL_MAKT, TL_T001W, TL_SAIDA.
*
*    LOOP AT ET_GOOD_CELLS INTO LS_GOOD
*       WHERE TABIX GT 0.
*
*      READ TABLE TG_SAIDA INTO WL_SAIDA INDEX LS_GOOD-ROW_ID.
*
*      APPEND WL_SAIDA TO TL_SAIDA.
*    ENDLOOP.
*
*    IF TL_SAIDA[] IS NOT INITIAL.
*      SELECT WERKS NAME1
*        FROM T001W
*        INTO TABLE TL_T001W
*         FOR ALL ENTRIES IN TL_SAIDA
*          WHERE WERKS EQ TL_SAIDA-WERKS.
*
*      SELECT MATNR MAKTX
*        FROM MAKT
*        INTO TABLE TL_MAKT
*         FOR ALL ENTRIES IN TL_SAIDA
*         WHERE MATNR EQ TL_SAIDA-MATNR.
*
*      SELECT MATNR MATKL
*        FROM MARA
*        INTO TABLE TL_MARA
*         FOR ALL ENTRIES IN TL_SAIDA
*         WHERE MATNR EQ TL_SAIDA-MATNR.
*
*    ENDIF.
*    SORT: TL_MAKT BY MATNR,
*          TL_MARA BY MATNR,
*          TL_T001W BY WERKS.
*
*    LOOP AT ET_GOOD_CELLS INTO LS_GOOD
*      WHERE TABIX GT 0.
*
*      READ TABLE TG_SAIDA INTO WL_SAIDA INDEX LS_GOOD-ROW_ID.
*
*      READ TABLE TL_MAKT INTO WL_MAKT
*        WITH KEY MATNR = WL_SAIDA-MATNR
*                 BINARY SEARCH.
*
*      READ TABLE TL_MARA INTO WL_MARA
*        WITH KEY MATNR = WL_SAIDA-MATNR
*                 BINARY SEARCH.
*
*      READ TABLE TL_T001W INTO WL_T001W
*        WITH KEY WERKS = WL_SAIDA-WERKS
*                 BINARY SEARCH.
*
*      MOVE: WL_MAKT-MAKTX TO WL_SAIDA-MAKTX,
*            WL_MARA-MATKL TO WL_SAIDA-MATKL,
*            WL_T001W-NAME1 TO WL_SAIDA-NAME1.
*
*      MODIFY TG_SAIDA FROM WL_SAIDA INDEX LS_GOOD-ROW_ID.
*      CLEAR: WL_SAIDA, WL_T001W, WL_MARA, WL_MAKT.
*    ENDLOOP.
*
*    PERFORM VERIFICA_ERROS.
*    CALL FUNCTION 'Z_DOC_CHECK_NEW'
*      EXPORTING
*        I_SCREEN      = '100'
*        I_SHOW        = SPACE
*        I_REPID       = SY-REPID
*        I_POPUP       = 1
**            i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
**            I_SET_FIELD   = 'X_FIELD'
*      IMPORTING
*        E_MESSAGEM    = WG_MENSAGEM
*      TABLES
*        IT_MSGS       = TG_MSG_RET.
*
**      read table tg_itens into wl_itens index ls_good-row_id.
**      if sy-subrc is initial.
**
***    LOOP AT tg_itens INTO wl_itens.
*****> Determina o CFOP
**        if wg_direitos-cfop is initial.
**          select single *
**            from marc
**            into wl_marc
**             where matnr eq wl_itens-matnr
**               and werks eq p_branch.
**
**          if sy-subrc is initial.
**            wl_itens-steuc = wl_marc-steuc.
**            select single *
**              from mbew
**              into wl_mbew
**               where matnr eq wl_itens-matnr
**                 and bwkey eq wl_itens-werks.
**
**            if sy-subrc is initial.
**              select single *
**                from j_1bbranch
**                 into wl_1bbranch
**                 where bukrs  eq p_bukrs
**                   and branch eq p_branch.
**
**              if sy-subrc is initial.
**                select single *
**                  from j_1baa
**                  into wl_1baa
**                   where nftype eq wg_fiscal-nftype.
**
**                if wl_1baa-entrad eq c_x.
**                  wl_direct = c_1.
**                else.
**                  wl_direct = c_2.
**                endif.
**
**                if wg_direitos-indcoper eq c_d.
**                  wl_dstcat = c_0.
**
**                else.
**                  wl_dstcat = c_1.
**
**                endif.
**
**                select single *
**                  from j_1bapn
**                  into wl_1bapn
**                   where direct eq wl_direct
**                     and dstcat eq wl_dstcat
**                     and indus3 eq wl_marc-indus
**                     and itmtyp eq wg_fiscal-itmtyp
**                     and ownpro eq wl_mbew-ownpr
**                     and matuse eq wl_mbew-mtuse
**                     and indus1 eq wl_1bbranch-industry.
**
**                if sy-subrc is initial.
**                  wl_itens-cfop = wl_1bapn-cfop.
**                else.
**                  clear: wl_itens-cfop.
**                endif.
**              else.
**                clear: wl_itens-cfop.
**              endif.
**            else.
**              clear: wl_itens-cfop.
**            endif.
**          else.
**            clear: wl_itens-cfop, wl_itens-steuc.
**          endif.
**        else.
**          select single *
**            from marc
**            into wl_marc
**             where matnr eq wl_itens-matnr
**               and werks eq p_branch.
**
**          wl_itens-steuc = wl_marc-steuc.
**          wl_itens-cfop = wg_direitos-cfop.
**        endif.
**
**        wl_itens-netwr = wl_itens-menge * wl_itens-netpr.
**        modify tg_itens from wl_itens index ls_good-row_id.
**
**      endif.
**    endloop.
**
***** Método de atualização de dados na Tela
**    call method grid1->refresh_table_display
**      exporting
**        is_stable = wa_stable.
**
**    perform verifica_erros.
  ENDMETHOD.                    "on_data_changed_finisheD
  METHOD on_onf4.


    TYPES: BEGIN OF tyl_mara,
             matnr TYPE mara-matnr,
             matkl TYPE setleaf-valfrom,
             mtart TYPE mtart,
           END OF tyl_mara,

           BEGIN OF tyl_field,
             tabname   TYPE dd03l-tabname,    "Nome da tabela
             fieldname TYPE dd03l-fieldname,    "Nome de campo
             s(1)      TYPE c,
           END OF tyl_field,

           BEGIN OF tyl_value,
             tabname    TYPE dd03l-tabname,    "Nome da tabela
             fieldname  TYPE dd03l-fieldname,    "Nome de campo
             char79(79) TYPE c,
           END OF tyl_value.

    DATA: BEGIN OF wl_matnr,
            matnr        TYPE mara-matnr,
            maktx        TYPE makt-maktx,
            meins        TYPE zsdt0036-meins,
            werks_fornec TYPE zsdt0036-werks_fornec,
            inco1        TYPE zsdt0036-inco1,
            safra        TYPE zsdt0036-safra,
          END OF wl_matnr,

          BEGIN OF wl_matnr_aux,
            field(50),
          END OF wl_matnr_aux,

          BEGIN OF wl_0036,
            val_de       TYPE zsdt0036-val_de,
            val_ate      TYPE zsdt0036-val_ate,
            dtvenc       TYPE zsdt0036-dtvenc,
            matnr        TYPE zsdt0036-matnr,
            waerk        TYPE zsdt0036-waerk,
            inco1        TYPE zsdt0036-inco1,
            safra        TYPE zsdt0036-safra,
            cultura      TYPE zsdt0036-cultura,
            meins        TYPE zsdt0036-meins,
            werks_fornec TYPE zsdt0036-werks_fornec,
            vlr_custo    TYPE zsdt0036-vlr_custo,
            perc_margem  TYPE zsdt0036-perc_margem,
            vlr_margem   TYPE zsdt0036-vlr_margem,
            vlr_venda    TYPE zsdt0036-vlr_venda,
            loekz        TYPE zsdt0036-loekz,
            bukrs        TYPE zsdt0036-bukrs,
          END OF wl_0036.

* RANGES: rg_safra for zsdt0036-safra.

    DATA: tl_matnr      LIKE TABLE OF wl_matnr,
          tl_matnr_aux  LIKE TABLE OF wl_matnr_aux,
          tl_return_tab TYPE TABLE OF ddshretval,
          wl_return_tab TYPE ddshretval,
          tl_dselc      TYPE TABLE OF dselc,
          tl_0036       LIKE TABLE OF wl_0036,              "zsdt0036,
*          wl_0036        TYPE zsdt0036,
          tl_makt       TYPE TABLE OF makt,
          wl_makt       TYPE makt,
          wl_itens      LIKE LINE OF tg_itens,
          tl_setleaf    TYPE TABLE OF setleaf,
          wl_setleaf    TYPE setleaf,
          tl_setlinet   TYPE TABLE OF setlinet,
          wl_setlinet   TYPE setlinet,
          tl_mara       TYPE TABLE OF tyl_mara,
          wl_mara       TYPE tyl_mara,
          tl_field      TYPE TABLE OF tyl_field,
          wl_field      TYPE tyl_field,
          tl_value      TYPE TABLE OF tyl_value,
          wl_value      TYPE tyl_value,
          wl_char(20),
          wl_index      TYPE sy-tabix,
          tl_dynpfields TYPE TABLE OF dynpread,
          wl_dynpfields TYPE dynpread.

    DATA: rgl_matnr   TYPE RANGE OF mara-matnr,
          wr_matnr    LIKE LINE OF rgl_matnr,
          rg_cultura  TYPE RANGE OF zsdt0036-cultura,
          wr_cultura  LIKE LINE OF rg_cultura,
          wl_zsdt0087 TYPE zsdt0087,
**** CS2022000324
          r_werks     TYPE RANGE OF werks_d,
          it_values   TYPE TABLE OF rgsb4,
          wa_values   TYPE rgsb4,
          wa_werks    LIKE LINE OF r_werks.
**** CS2022000324

*    RANGES: rg_safra for zsdt0036-safra.

    REFRESH: tl_matnr, rgl_matnr, tl_makt, tl_0036, tl_mara, tl_setlinet, tl_setleaf, tl_value, tl_field, tl_matnr_aux, tl_dynpfields,
             rg_cultura, it_values.
    CLEAR:   wl_matnr, rgl_matnr, wr_matnr, wl_makt, wl_itens, wl_0036, wl_return_tab,
             wl_setleaf, wl_setlinet, wl_mara, wl_field, wl_value, wl_matnr_aux, wl_dynpfields, wg_header-vlrtot.

**** CS2022000324
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        setnr         = 'MV45AFZZ_WERKS'
        table         = 'VBAP'
        class         = '0000'
        fieldname     = 'WERKS'
      TABLES
        set_values    = it_values
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.
    IF sy-subrc IS INITIAL.

      LOOP AT it_values INTO wa_values.
        wa_werks = 'IEQ'.
        wa_werks-low    = wa_values-from.
        IF wa_values-to IS NOT INITIAL.
          wa_werks = 'IBT'.
          wa_werks-high = wa_values-to.
        ENDIF.

        APPEND wa_werks TO r_werks.
      ENDLOOP.
    ENDIF.
**** CS2022000324


    IF wg_header-vkbur IS INITIAL.
      MESSAGE 'Escritório de Venda não Informado!' TYPE 'S'.
      EXIT.
    ENDIF.

    IF wg_header-cultura IS NOT INITIAL.
      wr_cultura-sign    = 'I'.
      wr_cultura-option  = 'EQ'.
      wr_cultura-low     = wg_header-cultura.

      APPEND wr_cultura TO rg_cultura.
      CLEAR: wr_cultura.
    ENDIF.
    wr_cultura-sign    = 'I'.
    wr_cultura-option  = 'EQ'.
    wr_cultura-low     =  space.

    APPEND wr_cultura TO rg_cultura.
    CLEAR: wr_cultura.

    SELECT val_de val_ate dtvenc matnr waerk inco1 safra cultura meins
           werks_fornec vlr_custo perc_margem vlr_margem vlr_venda loekz bukrs
      FROM zsdt0036
      INTO TABLE tl_0036
        WHERE "val_de  LE sy-datum
          "AND val_ate GE sy-datum
              loekz     EQ space
          AND waerk     EQ wg_header-waerk
*          AND cultura EQ wg_header-cultura
          AND cultura   IN rg_cultura
          AND safra     EQ wg_header-safra
          AND eliminado EQ space
          AND bukrs     EQ wa_t001k-bukrs.
*          AND safra   in rg_safra.
*          and werks   in rgl_werks.
    LOOP AT tl_0036 INTO wl_0036.
      IF wl_0036-cultura IS NOT INITIAL
      AND wl_0036-cultura NE wg_header-cultura.
        DELETE TABLE tl_0036 FROM wl_0036.
      ENDIF.

      IF  wl_0036-val_de  LE sy-datum
     AND  wl_0036-val_ate GE sy-datum.
      ELSE.
        DELETE TABLE tl_0036 FROM wl_0036.
      ENDIF.
    ENDLOOP.

    IF tl_0036[] IS NOT INITIAL.
      SELECT *
        FROM makt
        INTO TABLE tl_makt
         FOR ALL ENTRIES IN tl_0036
          WHERE matnr EQ tl_0036-matnr
            AND spras EQ sy-langu.

      SELECT matnr matkl mtart
        FROM mara
        INTO TABLE tl_mara
         FOR ALL ENTRIES IN tl_0036
         WHERE matnr EQ tl_0036-matnr.

    ENDIF.

    SORT: tl_makt BY matnr.

    LOOP AT tl_0036 INTO wl_0036.
      READ TABLE tl_makt INTO wl_makt
        WITH KEY matnr = wl_0036-matnr
                 BINARY SEARCH.
      MOVE-CORRESPONDING: wl_0036 TO wl_matnr.
      MOVE:  wl_makt-maktx        TO wl_matnr-maktx,
             wl_0036-werks_fornec TO wl_matnr-werks_fornec,
             wl_0036-inco1        TO wl_matnr-inco1.

      APPEND wl_matnr TO tl_matnr.

      MOVE: wl_matnr-matnr TO wl_matnr_aux-field.
      APPEND wl_matnr_aux  TO tl_matnr_aux.

      MOVE: wl_makt-maktx TO wl_matnr_aux-field.
      APPEND wl_matnr_aux TO tl_matnr_aux.

      MOVE: wl_0036-meins TO wl_matnr_aux-field.
      APPEND wl_matnr_aux TO tl_matnr_aux.

      MOVE: wl_0036-werks_fornec TO wl_matnr_aux-field.
      APPEND wl_matnr_aux TO tl_matnr_aux.

      MOVE: wl_0036-inco1 TO wl_matnr_aux-field.
      APPEND wl_matnr_aux TO tl_matnr_aux.

      CLEAR: wl_matnr, wl_makt, wl_matnr_aux.
    ENDLOOP.

    wl_field-tabname = 'MARA'.
    wl_field-fieldname = 'MATNR'.
    wl_field-s = 'X'.
    APPEND wl_field TO tl_field.


    wl_field-tabname = 'MAKT'.
    wl_field-fieldname = 'MAKTX'.
    wl_field-s = ' '.
    APPEND wl_field TO tl_field.

    wl_field-tabname = 'ZSDT0036'.
    wl_field-fieldname = 'MEINS'.
    wl_field-s = ' '.
    APPEND wl_field TO tl_field.

    wl_field-tabname = 'ZSDT0036'.
    wl_field-fieldname = 'WERKS_FORNEC'.
    wl_field-s = ' '.
    APPEND wl_field TO tl_field.

    wl_field-tabname = 'ZSDT0036'.
    wl_field-fieldname = 'INCO1'.
    wl_field-s = ' '.
    APPEND wl_field TO tl_field.

    CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
      EXPORTING
        cucol                     = 3
        curow                     = 15
        fieldname                 = 'MATNR'
        tabname                   = 'ZSDT0041'
      IMPORTING
        index                     = wl_index
        select_value              = wl_char
      TABLES
        fields                    = tl_field
        select_values             = tl_value
        valuetab                  = tl_matnr_aux
      EXCEPTIONS
        field_not_in_ddic         = 001
        more_then_one_selectfield = 002
        no_selectfield            = 003.



    IF sy-subrc IS INITIAL.
      READ TABLE tl_matnr INTO wl_matnr INDEX wl_index.
      IF es_row_no-row_id GT 0.
        READ TABLE tg_itens INTO wl_itens INDEX es_row_no-row_id.

        READ TABLE tl_makt INTO wl_makt
          WITH KEY matnr = wl_matnr-matnr.

        READ TABLE tl_mara INTO wl_mara
          WITH KEY matnr = wl_matnr-matnr.

*        MOVE WL_ITENS-ZWERT TO WL_ITENS-VL_UNIT.

        CLEAR wl_zsdt0087.
        CLEAR: wl_itens-spart, wl_itens-auart.
        SELECT SINGLE *
          FROM zsdt0087
          INTO wl_zsdt0087
          WHERE matkl = wl_mara-matkl
          AND   tpsim = wg_header-tpsim
          AND   inco1 = wl_matnr-inco1.
        IF sy-subrc = 0.
          MOVE: wl_zsdt0087-spart TO wl_itens-spart,
                wl_zsdt0087-auart TO wl_itens-auart.

          CASE wl_itens-auart.
            WHEN 'ZFTE' OR 'ZFUT'.
              MOVE: wg_header-safra TO wl_itens-charg.
            WHEN OTHERS.
              MOVE: '' TO wl_itens-charg.
          ENDCASE.

*          IF WL_ITENS-AUART EQ 'ZFTE'
*          OR WL_ITENS-AUART EQ 'ZFUT'.
*            MOVE: WG_HEADER-SAFRA TO WL_ITENS-CHARG.
*          ENDIF.

        ENDIF.

**** Clear no campo Charg CS2016001142>>>>>
        CASE wl_itens-auart.
          WHEN 'ZFTE' OR 'ZOFE'.
*            IF wl_matnr-werks_fornec EQ '0175' AND wl_mara-mtart EQ 'ZFER'.
            IF wl_matnr-werks_fornec IN r_werks AND wl_mara-mtart EQ 'ZFER'.
              CLEAR: wl_itens-charg.
            ENDIF.
        ENDCASE.
**** Clear no campo Charg CS2016001142<<<<<

**** Clear no campo Charg US109832-CS2023000286 - SC>>>>>
        CASE wl_itens-auart.
          WHEN 'ZFTE' OR 'ZOFE' OR 'ZFUT'.
            IF wl_itens-spart EQ '03'.
              CLEAR: wl_itens-charg.
            ENDIF.
        ENDCASE.
**** Clear no campo Charg US109832-CS2023000286 - SC>>>>>


        READ TABLE tl_0036 INTO wl_0036
          WITH KEY matnr        = wl_matnr-matnr
                   werks_fornec = wl_matnr-werks_fornec
                   inco1        = wl_matnr-inco1
                   meins        = wl_matnr-meins
                   safra        = wl_matnr-safra.

        MOVE: wl_matnr-matnr         TO wl_itens-matnr,
              wl_makt-maktx          TO wl_itens-maktx,
              wl_matnr-werks_fornec  TO wl_itens-werks,
              wl_matnr-inco1         TO wl_itens-inco1,
              wl_0036-dtvenc         TO wl_itens-dtvenc,
              wl_0036-meins          TO wl_itens-zieme.

        SELECT SINGLE vlr_frete
            FROM zsdt0037
            INTO (wl_itens-vlr_frete)
           WHERE val_de          LE sy-datum
             AND val_ate         GE sy-datum
             AND meins           EQ wl_itens-zieme
             AND filial_origem   EQ wl_itens-werks
             AND filial_destino  EQ wg_header-vkbur
             AND matkl           EQ wl_mara-matkl
             AND waers           EQ 'BRL'
             AND bukrs           EQ wa_t001k-bukrs.

        CASE wl_matnr-inco1.
          WHEN 'CIF' OR 'CPT' OR 'CFR'.
*            PERFORM ATUALIZA_FRETE CHANGING WL_ITENS-VLR_FRETE.
          WHEN OTHERS.
            wl_itens-vlr_frete = 0.
        ENDCASE.

        MODIFY tg_itens FROM wl_itens INDEX es_row_no-row_id.
      ENDIF.
    ENDIF.

    PERFORM calcula_itens.

*    BREAK ABAP.
    IF wg_header-tpsim(1) EQ c_tro(1).
      MOVE: 'WG_HEADER-TROTOTSC'        TO wl_dynpfields-fieldname,
            wg_header-trototsc          TO wl_dynpfields-fieldvalue.
      APPEND wl_dynpfields TO tl_dynpfields.

      MOVE: 'WG_HEADER-SCHA'        TO  wl_dynpfields-fieldname,
            wg_header-scha          TO  wl_dynpfields-fieldvalue.
      APPEND wl_dynpfields TO tl_dynpfields.

    ELSEIF wg_header-tpsim(1) EQ c_adt(1).
      MOVE: 'WG_HEADER-VLRTOT'        TO wl_dynpfields-fieldname,
            wg_header-vlrtot          TO wl_dynpfields-fieldvalue.
      APPEND wl_dynpfields TO tl_dynpfields.

      MOVE: 'WG_HEADER-COMPRSC'        TO  wl_dynpfields-fieldname,
            wg_header-comprsc          TO  wl_dynpfields-fieldvalue.
      APPEND wl_dynpfields TO tl_dynpfields.
    ELSE.
      MOVE: 'WG_HEADER-VLRTOT'        TO  wl_dynpfields-fieldname,
            wg_header-vlrtot          TO  wl_dynpfields-fieldvalue.
      APPEND wl_dynpfields TO tl_dynpfields.
    ENDIF.

    PERFORM vlrtot.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
      TABLES
        dynpfields = tl_dynpfields.

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = '/00'
      EXCEPTIONS
        function_not_supported = 1.

  ENDMETHOD.                                                "on_ONF4
  METHOD on_hotspot_click.

    DATA: wl_itens LIKE LINE OF tg_itens.
    DATA: wl_antec LIKE LINE OF tg_antec.
    DATA: tl_texto   TYPE catsxt_longtext_itab,
          wl_texto   TYPE LINE OF catsxt_longtext_itab,
          it_zib_err TYPE TABLE OF zib_contabil_err.

    IF e_row_id GT 0.
      IF sy-dynnr = '0108'.
        READ TABLE tg_antec INTO wl_antec INDEX e_row_id.
        IF e_column_id = 'ADIANT'.
          IF wl_antec-adiant = icon_operation.

          ELSEIF wl_antec-adiant = icon_message_error_small.
            SELECT *
               FROM zib_contabil_err
               INTO TABLE it_zib_err
               WHERE obj_key EQ wl_antec-obj_key.

            LOOP AT it_zib_err INTO DATA(wg_zib_err)  WHERE obj_key EQ wl_antec-obj_key.

              wl_texto = wg_zib_err-message.

              APPEND wl_texto TO tl_texto.
              CLEAR: wl_texto.
            ENDLOOP.
            IF tl_texto[] IS NOT INITIAL.
              CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
                EXPORTING
                  im_title        = 'Erros'
                  im_display_mode = c_x
                CHANGING
                  ch_text         = tl_texto.
            ENDIF.
            "
          ELSEIF wl_antec-adiant IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD wl_antec-adiant.
            SET PARAMETER ID 'GJR' FIELD wl_antec-gjahr.
            SET PARAMETER ID 'BUK' FIELD wl_antec-bukrs.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ELSEIF e_column_id = 'AUGBL' AND wl_antec-adiant IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD wl_antec-augbl.
            SET PARAMETER ID 'GJR' FIELD wl_antec-augdt(4).
            SET PARAMETER ID 'BUK' FIELD wl_antec-bukrs.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        ENDIF.
      ELSE.
        READ TABLE tg_itens INTO wl_itens INDEX e_row_id.

        IF wl_itens-vbeln IS NOT INITIAL.
          SET PARAMETER ID 'AUN' FIELD wl_itens-vbeln.
          CALL TRANSACTION  'VA03' AND SKIP FIRST SCREEN.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                                                "on_hotsot

  METHOD on_hotspot_click_tra.
    DATA: wl_trans LIKE LINE OF tg_trans.
    IF e_row_id GT 0.
      READ TABLE tg_trans INTO wl_trans INDEX e_row_id.

      IF e_column_id =  'VBELN' AND wl_trans-vbeln IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD wl_trans-vbeln.
        CALL TRANSACTION  'VA03' AND SKIP FIRST SCREEN.
      ELSEIF e_column_id =  'VBELV' AND wl_trans-vbelv IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD wl_trans-vbelv.
        CALL TRANSACTION  'VA03' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.
  ENDMETHOD.                                                "on_hotsot
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: BEGIN OF tl_ucomm OCCURS 0,
          ucomm TYPE  sy-ucomm,
        END OF tl_ucomm,

        wl_0040   TYPE zsdt0040,
        wl_0041   TYPE zsdt0041,
        var_vbeln TYPE c,
        usuario   TYPE sy-msgv1.

  DATA: wl_0043  TYPE zsdt0043,
        wl_itens LIKE LINE OF tg_itens.

  REFRESH:tl_ucomm.
  CLEAR: tl_ucomm, wl_0040, wl_0043, wl_0041, var_vbeln.

  LOOP AT tg_itens WHERE NOT vbeln IS INITIAL.
    MOVE abap_true TO var_vbeln.
  ENDLOOP.

  PERFORM carrega_antec USING tg_antec[] wg_header-doc_simulacao.
  DELETE tg_antec WHERE estorno = 'X'.
  DELETE tg_antec WHERE augbl IS NOT INITIAL.
  IF tg_antec[] IS INITIAL.
    APPEND VALUE #( ucomm = 'BOLETO' ) TO tl_ucomm[].
  ENDIF.

  APPEND VALUE #( ucomm = c_print ) TO tl_ucomm[].
  APPEND VALUE #( ucomm = c_aprov ) TO tl_ucomm[].
  APPEND VALUE #( ucomm = c_reprov ) TO tl_ucomm[].
  APPEND VALUE #( ucomm = c_bloq ) TO tl_ucomm[].
  APPEND VALUE #( ucomm = c_gera ) TO tl_ucomm[].
  APPEND VALUE #( ucomm = c_printc ) TO tl_ucomm[].
  APPEND VALUE #( ucomm = c_sacas ) TO tl_ucomm[].
  APPEND VALUE #( ucomm = c_dsc_abs ) TO tl_ucomm[].
  APPEND VALUE #( ucomm = c_vinc_desc ) TO tl_ucomm[].
  APPEND VALUE #( ucomm = c_recalc ) TO tl_ucomm[].

  APPEND VALUE #( ucomm = c_altcsa ) TO tl_ucomm[].


*Início - Sara Oikawa - 38859 - Agosto/2020
  APPEND VALUE #( ucomm = c_altjur ) TO tl_ucomm[].
  APPEND VALUE #( ucomm = c_altadt ) TO tl_ucomm[].
*Fim - Sara Oikawa - 38859 - Agosto/2020

  " 03.01.2022 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- >
  APPEND VALUE #( ucomm = c_boleta_vin ) TO tl_ucomm[].
  " 03.01.2022 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- <

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
*  APPEND VALUE #( ucomm = c_altcsa ) TO tl_ucomm[]. "//CS2021000715 #62000 Exibir para todos os Usuarios.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  APPEND VALUE #( ucomm = c_altcpg ) TO tl_ucomm[].
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

  IF line_exists( tg_trans[
                            doc_simulacao = wg_header-doc_simulacao
                            categoria = 'O'
                            flag = abap_false
                            estorno = abap_false
                          ] ).
    DELETE tl_ucomm WHERE ucomm EQ c_vinc_desc.
  ENDIF.

  IF wg_status EQ icon_release OR
     wg_status EQ icon_gis_pan.
    APPEND VALUE #( ucomm = c_modif ) TO tl_ucomm[].
  ENDIF.

  CLEAR c_erro.
  PERFORM f_val_sacas_user CHANGING c_erro.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'SAVE' OR 'SEARCH'.
    WHEN OTHERS.
*    WHEN 'ADD' OR 'MODIF' OR 'COPY' OR 'ATUAL' OR 'PRINT' OR 'SHOW_MSGRE' OR
*         'APROV' OR 'REPROV' OR 'BLOQ' OR 'GERA' OR 'PRINTC' OR 'BOLETO' OR
*         'QTD_SACA' OR 'VINC_DESC' OR 'SHOW_LOG' OR 'RECALC'.

      IF NOT wg_header-doc_simulacao IS INITIAL AND s_acao IS INITIAL.

*    Realiza o desbloqueio das ação enteriores e bloqueia novamente para não acumular Bloqueio.
        CALL FUNCTION 'DEQUEUE_EZSDT0040'
          EXPORTING
            doc_simulacao = wg_header-doc_simulacao.

        CALL FUNCTION 'ENQUEUE_EZSDT0040'
          EXPORTING
            doc_simulacao  = wg_header-doc_simulacao
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc <> 0.

          usuario = sy-msgv1.

          MOVE: c_modif TO tl_ucomm.
          APPEND tl_ucomm.
          CLEAR: tl_ucomm.

          LOOP AT t_fieldcatalog INTO w_fieldcatalog.
            w_fieldcatalog-edit = abap_false.
            MODIFY t_fieldcatalog FROM w_fieldcatalog.
          ENDLOOP.

          CALL METHOD grid1->set_frontend_fieldcatalog
            EXPORTING
              it_fieldcatalog = t_fieldcatalog.

          CALL METHOD cl_gui_cfw=>dispatch.
          SET PF-STATUS 'Z001' EXCLUDING tl_ucomm.
          SET TITLEBAR 'Z001'.

          CLEAR: tl_ucomm.
          MESSAGE s836(sd) WITH |Doc. Simulação { wg_header-doc_simulacao } Bloqueado por { usuario }!| .
          EXIT.
        ENDIF.
      ENDIF.

  ENDCASE.


  DATA(count) = REDUCE int4( INIT y = 0 FOR ls2 IN tg_itens WHERE ( vbeln IS NOT INITIAL ) NEXT y = y + 1 ).

  IF count IS INITIAL.
    IF vta_sistpro NE vlr_total AND wg_status NE icon_gis_pan. " // ICON_GIS_PAN equivalente a Bloqueado
      IF wg_acao NE 'MODIF' AND wg_acao NE 'ADD'.
        CASE sy-ucomm.
          WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'SAVE' OR 'SEARCH'.
          WHEN OTHERS.
* Início - Sara Oikawa - 38859 - Agosto/2020
            IF wg_header-tpsim EQ 'TS' OR wg_header-tpsim EQ 'TV'.
* Fim - Sara Oikawa - 38859 - Agosto/2020
              DELETE tl_ucomm WHERE ucomm EQ c_recalc.
            ENDIF.
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDIF.

  IF wg_status NE icon_gis_pan.

    IF wg_acao NE c_add
    AND wg_acao NE c_modif.

      MOVE: c_save TO tl_ucomm.
      APPEND tl_ucomm.
      CLEAR: tl_ucomm.

      IF c_erro IS INITIAL.
        CASE wg_save.
          WHEN c_sacas OR c_dsc_abs.
            DELETE tl_ucomm WHERE ucomm EQ c_save.
*Início - Sara Oikawa - 38859 - Agosto/2020
          WHEN c_altjur.
            DELETE tl_ucomm WHERE ucomm EQ c_save.

          WHEN c_altadt.
            DELETE tl_ucomm WHERE ucomm EQ c_save.
*Fim - Sara Oikawa - 38859 - Agosto/2020
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
* Inicio - wsb CS2021000715
*          WHEN c_altcsa.
*            DELETE tl_ucomm WHERE ucomm EQ c_save.
*            IF grid1 IS NOT INITIAL.
*
*              LOOP AT t_fieldcatalog INTO w_fieldcatalog
*                 WHERE fieldname EQ 'CULTURA_APL'
*                    OR fieldname EQ 'SAFRA_APL'.
*                w_fieldcatalog-edit = c_x.
*                MODIFY t_fieldcatalog FROM w_fieldcatalog.
*              ENDLOOP.
*
*              CALL METHOD grid1->set_frontend_fieldcatalog
*                EXPORTING
*                  it_fieldcatalog = t_fieldcatalog.
*            ENDIF.
* Fim - wsb CS2021000715
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
          WHEN c_altcpg.
            DELETE tl_ucomm WHERE ucomm EQ c_save.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

        ENDCASE.
      ENDIF.

      CASE wg_save.
        WHEN c_altcsa.
          DELETE tl_ucomm WHERE ucomm EQ c_save.
          IF grid1 IS NOT INITIAL.

            LOOP AT t_fieldcatalog INTO w_fieldcatalog
               WHERE fieldname EQ 'CULTURA_APL'
                  OR fieldname EQ 'SAFRA_APL'.
              w_fieldcatalog-edit = c_x.
              MODIFY t_fieldcatalog FROM w_fieldcatalog.
            ENDLOOP.

            CALL METHOD grid1->set_frontend_fieldcatalog
              EXPORTING
                it_fieldcatalog = t_fieldcatalog.
          ENDIF.
      ENDCASE.

      IF wg_acao EQ c_atual.

        IF c_erro IS INITIAL.
          CASE wg_header-tpsim.
            WHEN 'TS' OR 'TV'.
              DELETE tl_ucomm WHERE ucomm EQ c_sacas.
*Início - Sara Oikawa - 38859 - Agosto/2020
            WHEN 'AD'.
              DELETE tl_ucomm WHERE ucomm EQ c_altjur.
              DELETE tl_ucomm WHERE ucomm EQ c_altadt.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
*            WHEN 'PM' OR 'VF' OR 'VP' OR 'VV' .
            WHEN 'PM' OR 'VF'.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
              DELETE tl_ucomm WHERE ucomm EQ c_altjur.
*Fim - Sara Oikawa - 38859 - Agosto/2020

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
            WHEN 'VP' OR 'VV' .
              DELETE tl_ucomm WHERE ucomm EQ c_altjur.
              DELETE tl_ucomm WHERE ucomm EQ c_altcpg.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
          ENDCASE.

        ENDIF.

        SELECT SINGLE *
          FROM zsdt0040
          INTO wl_0040
           WHERE doc_simulacao EQ wg_header-doc_simulacao.

        IF sy-subrc IS INITIAL.

          DELETE tl_ucomm WHERE ucomm EQ c_print.

          CASE wl_0040-status.
            WHEN c_a.

              SELECT SINGLE *
               FROM zsdt0041
                INTO wl_0041
                WHERE doc_simulacao EQ wg_header-doc_simulacao
                AND vbeln         EQ space.

              IF sy-subrc IS INITIAL.
                DELETE tl_ucomm WHERE ucomm EQ c_gera.
              ELSE.

                IF c_erro IS INITIAL.
                  DELETE tl_ucomm WHERE ucomm EQ c_dsc_abs.
                ENDIF.

                DELETE tl_ucomm WHERE ucomm EQ c_printc.
              ENDIF.
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
* Inicio - wsb CS2021000715
              DELETE tl_ucomm WHERE ucomm EQ c_altcsa.
* Fim - wsb CS2021000715
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

*Início - Sara Oikawa - 38859 - Agosto/2020
            WHEN OTHERS.
              APPEND VALUE #( ucomm = c_altjur ) TO tl_ucomm[].

              APPEND VALUE #( ucomm = c_altadt ) TO tl_ucomm[].
*Fim - Sara Oikawa - 38859 - Agosto/2020

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
              APPEND VALUE #( ucomm = c_altcpg ) TO tl_ucomm[].
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

          ENDCASE.

          IF sy-ucomm NE 'SAVE'.

            CLEAR: wl_0043.
            SELECT SINGLE *
              FROM zsdt0043
              INTO wl_0043
               WHERE werks EQ wl_0040-vkbur
                 AND uname EQ sy-uname.

            IF sy-subrc IS INITIAL.
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077
              IF wg_header-ernam IS NOT INITIAL.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077

                IF wl_0040-status NE c_a AND wg_header-ernam NE sy-uname.

                  DELETE tl_ucomm WHERE ucomm EQ c_aprov.
                ENDIF.

*          Se não tem nenhum VBELN na Solicitação o Botão C_BLOQ e o C_REPROV é ATIVADO.
                IF var_vbeln IS INITIAL.
                  DELETE tl_ucomm WHERE ucomm EQ c_bloq.
                  DELETE tl_ucomm WHERE ucomm EQ c_reprov.

*            IF WL_0040-STATUS EQ C_A.
*
*            ENDIF.

                ENDIF.

                " 05.01.2022 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- >
                DELETE tl_ucomm WHERE ucomm EQ c_boleta_vin.
                " 05.01.2022 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- <
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077
              ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077



            ENDIF.
          ENDIF.

          IF grid1 IS NOT INITIAL.

            LOOP AT t_fieldcatalog INTO w_fieldcatalog.
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
              IF w_fieldcatalog-fieldname EQ 'CULTURA_APL' OR
                 w_fieldcatalog-fieldname EQ 'SAFRA_APL'.
                w_fieldcatalog-edit = c_x.
              ELSE.
                w_fieldcatalog-edit = space.
                MODIFY t_fieldcatalog FROM w_fieldcatalog.
              ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
            ENDLOOP.

            CALL METHOD grid1->set_frontend_fieldcatalog
              EXPORTING
                it_fieldcatalog = t_fieldcatalog.
          ENDIF.

        ENDIF.

*        BREAK WBARBOSA.

        SELECT SINGLE a~*
            FROM zsdt0040 AS a
          INTO @DATA(w_0040)
          WHERE a~doc_simulacao EQ @wg_header-doc_simulacao.

        IF sy-subrc IS INITIAL.

          SELECT SINGLE *
            FROM zsdt0043
            INTO @DATA(w_0043)
             WHERE werks EQ @wg_header-vkbur
               AND uname EQ @sy-uname.

*          IF WG_HEADER-ERNAM IS INITIAL AND WG_HEADER-ERDAT IS NOT INITIAL .
*            DELETE TL_UCOMM WHERE UCOMM EQ C_APROV.
*          ELSE.

          IF sy-subrc = 0.
            IF w_0040-ernam EQ sy-uname.

              " 21.02.2023 - RAMON - 102323 -->
              IF w_0040-ecommerce = 'X'.
                DELETE tl_ucomm WHERE ucomm EQ c_aprov.
              ENDIF.
              " 21.02.2023 - RAMON - 102323 --<

            ELSE.
              DELETE tl_ucomm WHERE ucomm EQ c_aprov.
            ENDIF.
          ENDIF.
*          ENDIF.

          IF w_0040-status EQ c_a.
            APPEND VALUE #( ucomm = c_aprov ) TO tl_ucomm[].
          ENDIF.
        ENDIF.
      ENDIF.

    ELSEIF wg_acao EQ c_add
        OR wg_acao EQ c_modif.

      MOVE: c_atual TO tl_ucomm.

      APPEND tl_ucomm.
      CLEAR: tl_ucomm.

      IF grid1 IS NOT INITIAL.

        LOOP AT t_fieldcatalog INTO w_fieldcatalog
           WHERE fieldname EQ 'MATNR'
              OR fieldname EQ 'ZMENG'
              OR fieldname EQ 'DTVENC'
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
              OR fieldname EQ 'CULTURA_APL'
              OR fieldname EQ 'SAFRA_APL'.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
          w_fieldcatalog-edit = c_x.
          MODIFY t_fieldcatalog FROM w_fieldcatalog.
        ENDLOOP.


        CLEAR: wl_0043.
        SELECT SINGLE *
          FROM zsdt0043
          INTO wl_0043
           WHERE werks EQ wg_header-vkbur
             AND uname EQ sy-uname.

        IF sy-subrc IS INITIAL.

          LOOP AT t_fieldcatalog INTO w_fieldcatalog
           WHERE fieldname EQ 'DESCONTO'
              OR fieldname EQ 'ZWERT'.

            w_fieldcatalog-edit = c_x.
            MODIFY t_fieldcatalog FROM w_fieldcatalog.

          ENDLOOP.

          LOOP AT SCREEN.
            IF screen-group2 EQ 'A3'.
              screen-input = 1.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.

        ELSEIF sy-subrc IS NOT INITIAL
           AND wg_acao EQ c_modif.

          REFRESH: style, tg_itens-style.

        ENDIF.

        CALL METHOD grid1->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = t_fieldcatalog.
      ENDIF.
    ENDIF.
  ELSE.
    LOOP AT t_fieldcatalog INTO w_fieldcatalog.
      w_fieldcatalog-edit = abap_false.
      MODIFY t_fieldcatalog FROM w_fieldcatalog.
    ENDLOOP.

    CALL METHOD grid1->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog.

  ENDIF.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

  IF wg_acao EQ c_modif AND
     wg_save EQ c_altcpg.
    LOOP AT t_fieldcatalog INTO w_fieldcatalog.
      w_fieldcatalog-edit = abap_false.
      MODIFY t_fieldcatalog FROM w_fieldcatalog.
    ENDLOOP.

    CALL METHOD grid1->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog.
  ENDIF.

* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8


  IF wg_acao EQ c_modif.

    MOVE: c_add TO tl_ucomm.
    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

    MOVE: c_copy TO tl_ucomm.
    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

  ENDIF.

*  BREAK WBARBOSA.
*  IF WG_ACAO IS NOT INITIAL.
*    IF WG_HEADER-ERNAM IS INITIAL.
*      DELETE TL_UCOMM WHERE UCOMM EQ C_APROV.
*    ELSE.
*      SELECT SINGLE A~*
*        FROM ZSDT0040 AS A
*        INNER JOIN ZSDT0043 AS B ON B~WERKS EQ A~VKBUR
*                                AND B~UNAME EQ A~ERNAM
*        INTO @DATA(W_0040)
*         WHERE A~DOC_SIMULACAO EQ @WG_HEADER-DOC_SIMULACAO.
*
*      IF W_0040-ERNAM EQ SY-UNAME.
*        DELETE TL_UCOMM WHERE UCOMM EQ C_APROV.
*      ENDIF.
*
*    ENDIF.
*  ENDIF.

  " 21.02.2023 - RAMON - 102323 -->
  IF wg_header-ecommerce = 'X'.
    APPEND VALUE #( ucomm = 'BOLETO' ) TO tl_ucomm[].
  ENDIF.
  " 21.02.2023 - RAMON - 102323 --<


  CALL METHOD cl_gui_cfw=>dispatch.
  SET PF-STATUS 'Z001' EXCLUDING tl_ucomm.
  SET TITLEBAR 'Z001'.

  CLEAR: tl_ucomm.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA mtart TYPE mtart.

  " 03.01.2022 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- >
  DATA lv_erro.
  " 03.01.2022 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL <--

*Início - Sara Oikawa - 38859 - Agosto/2020
  DATA lv_resp.
*Fim - Sara Oikawa - 38859 - Agosto/2020

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  DATA: lv_msg TYPE char255.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

**** CS2022000324
  DATA: r_werks   TYPE RANGE OF werks_d,
        it_values TYPE TABLE OF rgsb4,
        wa_values TYPE rgsb4,
        wa_werks  LIKE LINE OF r_werks.

  REFRESH it_values.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr         = 'MV45AFZZ_WERKS'
      table         = 'VBAP'
      class         = '0000'
      fieldname     = 'WERKS'
    TABLES
      set_values    = it_values
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.
  IF sy-subrc IS INITIAL.

    LOOP AT it_values INTO wa_values.
      wa_werks = 'IEQ'.
      wa_werks-low    = wa_values-from.
      IF wa_values-to IS NOT INITIAL.
        wa_werks = 'IBT'.
        wa_werks-high = wa_values-to.
      ENDIF.

      APPEND wa_werks TO r_werks.
    ENDLOOP.
  ENDIF.
**** CS2022000324

  CASE sy-ucomm.
    WHEN c_add.
* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF wg_save IS NOT INITIAL AND
          wg_save NE c_add.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Criar Simulação'.
        MESSAGE i001(zsd) DISPLAY LIKE 'E' WITH TEXT-e53 TEXT-e54 TEXT-e87.
        sy-ucomm = wg_save.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7

      IF wg_flag IS INITIAL.

*    Realiza o desbloqueio das ação enteriores e bloqueia novamente para não acumular Bloqueio.
        CALL FUNCTION 'DEQUEUE_EZSDT0040'
          EXPORTING
            doc_simulacao = wg_header-doc_simulacao.

        PERFORM limpa_variavel USING sy-ucomm.

        " 21.02.2023 - RAMON - 102323 -->
        PERFORM f_init_add.
        " 21.02.2023 - RAMON - 102323 --<

*        PERFORM GET_NEXT_NUMBER  USING  'ZSIMULACAO'
*                                        '1'
*                               CHANGING WG_HEADER-DOC_SIMULACAO.

        PERFORM get_next_number  USING 'ZNR_SOL_OV' "--'ZSIMULACAO'
                               '01' CHANGING wg_header-doc_simulacao.


        MOVE: sy-uname TO wg_header-ernam,
              sy-datum TO wg_header-erdat,
              sy-uzeit TO wg_header-erzet.
      ENDIF.
      MOVE c_add TO wg_acao.

    WHEN c_atual.
* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF wg_save IS NOT INITIAL AND
          wg_save NE c_atual.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Buscar Simulação'.
        MESSAGE i001(zsd) DISPLAY LIKE 'E' WITH TEXT-e53 TEXT-e54 TEXT-e91.
        sy-ucomm = wg_save.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7

      PERFORM limpa_variavel USING sy-ucomm.
      PERFORM busca_dados_doc.
      PERFORM busca_dados.
      MOVE: c_atual TO wg_acao.

    WHEN c_search.
      PERFORM busca_dados.

* Início - Sara Oikawa - 38859 - Agosto/2020
      IF wg_save EQ c_altjur.
        sy-ucomm = c_altjur.
      ENDIF.

      IF wg_save EQ c_altadt.
        sy-ucomm = c_altadt.
      ENDIF.
* Fim - Sara Oikawa - 38859 - Agosto/2020

    WHEN c_copy.
      wg_copy = abap_true.
* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF wg_save IS NOT INITIAL AND
          wg_save NE c_copy.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Copiar Simulação'.
        MESSAGE i001(zsd) DISPLAY LIKE 'E' WITH TEXT-e53 TEXT-e54 TEXT-e89.
        sy-ucomm = wg_save.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7


**    Valida se existe documento para ser modificado.
      SELECT SINGLE doc_simulacao ecommerce
        FROM zsdt0040
        INTO (wg_header-doc_simulacao,wg_header-ecommerce)
         WHERE doc_simulacao EQ wg_header-doc_simulacao.

      IF sy-subrc IS INITIAL.

        " 21.02.2023 - RAMON - 102323 -->
        IF wg_header-ecommerce = 'X'.

          PERFORM f_check_user_ecomm CHANGING gv_info.

          IF gv_info IS INITIAL.
            MESSAGE s836(sd) DISPLAY LIKE 'E'
              WITH 'Usuário sem permissão para'
                   'Copiar Venda com origem E-commerce'.
            EXIT.
          ENDIF.

        ENDIF.
        " 21.02.2023 - RAMON - 102323 --<

        PERFORM limpa_variavel USING c_atual.
        PERFORM busca_dados_doc.
        PERFORM busca_dados.

*    Realiza o desbloqueio das ação enteriores e bloqueia novamente para não acumular Bloqueio.
        CALL FUNCTION 'DEQUEUE_EZSDT0040'
          EXPORTING
            doc_simulacao = wg_header-doc_simulacao.

        PERFORM get_next_number  USING 'ZNR_SOL_OV'
                               '01' CHANGING wg_header-doc_simulacao.

*        PERFORM GET_NEXT_NUMBER  USING 'ZSIMULACAO'
*                                       '1' CHANGING WG_HEADER-DOC_SIMULACAO.

        MOVE: sy-uname TO wg_header-ernam,
              sy-datum TO wg_header-erdat,
              sy-uzeit TO wg_header-erzet.

        PERFORM zf_determine_funrural USING abap_true. "Forçar determinar Funrural

        LOOP AT tg_itens.
          CLEAR: tg_itens-vbeln, tg_itens-style, tg_itens-desconto, tg_itens-vlr_ajuste,
                 tg_itens-vl_unit, tg_itens-zwert, tg_itens-desc_absoluto, tg_itens-vlrtot, tg_itens-vlr_frete,
                 wg_header-dt_entrega_sem, wg_header-dt_entrega_def, wg_header-dt_entrega_fet.

          SELECT SINGLE mtart
            FROM mara
            INTO mtart
            WHERE matnr EQ tg_itens-matnr.

**** Clear no campo Charg CS2016001142>>>>>
          CASE tg_itens-auart.
            WHEN 'ZFTE' OR 'ZOFE'.
*              IF tg_itens-werks EQ '0175' AND mtart EQ 'ZFER'.
              IF tg_itens-werks IN r_werks AND mtart EQ 'ZFER'.
                CLEAR tg_itens-charg.
              ENDIF.
          ENDCASE.
**** Clear no campo Charg CS2016001142<<<<<

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
          IF tg_itens-cultura_apl IS INITIAL.
            IF wg_cultura_apl IS NOT INITIAL.
              tg_itens-cultura_apl = wg_cultura_apl.
            ENDIF.
          ENDIF.

          IF tg_itens-safra_apl IS INITIAL.
            tg_itens-safra_apl = wg_header-safra.
          ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

          MODIFY tg_itens.
        ENDLOOP.

        MOVE: icon_initial TO wg_status.
        MOVE c_modif TO wg_acao.

      ENDIF.

      " 06.02.2023 - TENTATIVA NAO DAR ERRO AO COPIAR -->
***      IF grid1 IS BOUND.
***
***        PERFORM calcula_itens.
***        PERFORM vlrtot.
***
***        CALL METHOD grid1->check_changed_data.
***        CALL METHOD grid1->refresh_table_display.
***
***      ENDIF.
      " 06.02.2023 - TENTATIVA NAO DAR ERRO AO COPIAR -->

    WHEN c_descp.
      PERFORM inputa_desc.

    WHEN c_modif.

**    Valida se existe documento para ser modificado.
      SELECT SINGLE doc_simulacao ecommerce
        FROM zsdt0040
        INTO (wg_header-doc_simulacao,wg_header-ecommerce)
         WHERE doc_simulacao EQ wg_header-doc_simulacao
           AND ( status NE c_a AND status NE c_b ).

      IF sy-subrc IS INITIAL.

        " 21.02.2023 - RAMON - 102323 -->
        IF wg_header-ecommerce = 'X'.

          PERFORM f_check_user_ecomm CHANGING gv_info.

          IF gv_info IS INITIAL.
            MESSAGE s836(sd) DISPLAY LIKE 'E'
              WITH 'Usuário sem permissão para'
                   'Editar Venda com origem E-commerce'.
            EXIT.
          ENDIF.

        ENDIF.
        " 21.02.2023 - RAMON - 102323 --<

*    Realiza o desbloqueio das ação enteriores e bloqueia novamente para não acumular Bloqueio.
        CALL FUNCTION 'DEQUEUE_EZSDT0040'
          EXPORTING
            doc_simulacao = wg_header-doc_simulacao.

        CALL FUNCTION 'ENQUEUE_EZSDT0040'
          EXPORTING
            doc_simulacao  = wg_header-doc_simulacao
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        PERFORM limpa_variavel USING c_atual.
        PERFORM busca_dados_doc.
        PERFORM busca_dados.

        MOVE: icon_initial TO wg_status.
        MOVE c_modif TO wg_acao.
      ELSE.
        MESSAGE s836(sd)
        DISPLAY LIKE 'E'
        WITH |Simulador esta com Status de { COND #( WHEN c_b EQ 'B' THEN 'Bloqueado' ELSE 'Aprovado' ) }|.
      ENDIF.

    WHEN c_tpord.
* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
      IF wg_save EQ c_altcpg.
*        IF WG_HEADER-TPSIM NE 'TS' AND WG_HEADER-TPSIM  NE 'TV' AND WG_HEADER-TPSIM  NE 'AD'.
*          MESSAGE S836(SD)
*          DISPLAY LIKE 'E'
*          WITH 'Alteração de Condição de Pagamento não Permitida.'.
*          WG_HEADER-TPSIM = WG_TPSIM_ANT.
**          MOVE C_ATUAL TO WG_ACAO.
**          clear sy-ucomm.
*          EXIT.
*        ENDIF.
        IF wg_header-tpsim EQ 'TS' OR wg_header-tpsim  EQ 'TV' OR wg_header-tpsim  EQ 'AD'.
          MOVE c_modif TO wg_acao.
        ENDIF.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
      PERFORM limpa_variavel USING sy-ucomm.

    WHEN c_cancel.
      CALL FUNCTION 'DEQUEUE_EZSDT0040'
        EXPORTING
          doc_simulacao = wg_header-doc_simulacao.
      PERFORM limpa_variavel USING sy-ucomm.

    WHEN c_show_msgre.
      PERFORM verifica_erros.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen    = '100'
          i_show      = 'X'
          i_repid     = sy-repid
          i_popup     = 0
*         i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
          i_set_field = 'X_FIELD'
          i_set_cell  = 'WG_CELL'
          i_set_obj   = 'WG_OBJ'
        IMPORTING
          e_messagem  = wg_mensagem
        TABLES
          it_msgs     = tg_msg_ret.

    WHEN c_save.
      CALL METHOD grid1->check_changed_data.
      PERFORM verifica_erros.

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
      " Quando se clicar no Botão Alterar Cultura/Safra de aplicação,
      " O sistema deve deixar Gravar, mesmo se houver outras mensagens de erro
      " (Obs.: exceto onde a mensagem de erro é referente a Cultura/Safra)
      IF wg_save EQ c_altcsa.
        DELETE tg_msg_ret WHERE ( msg NS TEXT-e67 AND  msg NS TEXT-e68 AND  msg NS TEXT-e69 ).
      ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

      IF tg_msg_ret[] IS INITIAL.

*Início - Sara Oikawa - 38859 - Agosto/2020
        IF wg_save EQ 'ALTADT'.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              text_question         = TEXT-i03  "Confirma Alterações de Adiantamento?'
              text_button_1         = 'Sim'
              text_button_2         = 'Não'
              display_cancel_button = ' '
            IMPORTING
              answer                = lv_resp.

          IF lv_resp EQ 1.
            PERFORM grava_dados.
          ENDIF.

        ELSE.
*Fim - Sara Oikawa - 38859 - Agosto/2020

          PERFORM grava_dados.
        ENDIF.

      ELSE.

        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.

      ENDIF.

      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen    = '100'
          i_show      = 'X'
          i_repid     = sy-repid
          i_popup     = 0
*         i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
          i_set_field = 'X_FIELD'
          i_set_cell  = 'WG_CELL'
          i_set_obj   = 'WG_OBJ'
        IMPORTING
          e_messagem  = wg_mensagem
        TABLES
          it_msgs     = tg_msg_ret.

    WHEN 'INFOR'.
      PERFORM monta_proposta.
      CALL SCREEN 102
*        ENDING AT 51 9
        STARTING AT 3 3.

    WHEN 'PRINT'.
* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF wg_save IS NOT INITIAL AND
          wg_save NE 'PRINT'.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Imprimir Simulação'.
        MESSAGE i001(zsd) DISPLAY LIKE 'E' WITH TEXT-e53 TEXT-e54 TEXT-e92.
        sy-ucomm = wg_save.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7

      PERFORM limpa_variavel USING c_atual.
      PERFORM busca_dados_doc.
      PERFORM busca_dados.
      PERFORM imprime_simulacao USING wg_header-doc_simulacao.
      wg_acao = c_atual.

    WHEN 'PRINTC'.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF wg_save IS NOT INITIAL AND
          wg_save NE 'PRINTC'.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Gerar Contrato'.
        MESSAGE i001(zsd) DISPLAY LIKE 'E' WITH TEXT-e53 TEXT-e54 TEXT-e96.
        sy-ucomm = wg_save.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7

      PERFORM imprimi_contrato.

      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          functioncode           = '/00'
        EXCEPTIONS
          function_not_supported = 1.

    WHEN 'BACK'
      OR 'EXIT'.
      CALL FUNCTION 'DEQUEUE_EZSDT0040'
        EXPORTING
          doc_simulacao = wg_header-doc_simulacao.
      LEAVE TO SCREEN 0.

    WHEN c_aprov.

      PERFORM busca_dados_doc.
      PERFORM busca_dados.

      PERFORM verifica_erros.

      IF tg_msg_ret[] IS INITIAL.

        "PERFORM salva_imposto.
        PERFORM modifica_status USING 'A'.

        " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
        IF lv_erro IS INITIAL.
          PERFORM salva_imposto.
        ENDIF.
        " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

      ELSE.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen    = '100'
            i_show      = 'X'
            i_repid     = sy-repid
            i_popup     = 0
            i_set_field = 'X_FIELD'
          IMPORTING
            e_messagem  = wg_mensagem
          TABLES
            it_msgs     = tg_msg_ret.
      ENDIF.

    WHEN c_reprov.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF wg_save IS NOT INITIAL AND
          wg_save NE c_reprov.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Reprovar'.
        MESSAGE i001(zsd) DISPLAY LIKE 'E' WITH TEXT-e53 TEXT-e54 TEXT-e93.
        sy-ucomm = wg_save.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7

      PERFORM modifica_status USING 'R'.

    WHEN c_mglob.
      PERFORM monta_mglobal.
      PERFORM mota_layout_mglobal.
      PERFORM exibe_mgblobal.

    WHEN c_memo.
      PERFORM monta_memo.
      CALL SCREEN 101 ENDING AT 51 13 STARTING AT 3 3.

    WHEN 'PG_ANTE'.
      PERFORM carrega_antec USING tg_antec[] wg_header-doc_simulacao.
      CALL SCREEN 108. " ENDING AT 165 10 STARTING AT 3 3.

    WHEN 'BOLETO'.
      DATA: wl_boleto TYPE j_1bdocnum.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF wg_save IS NOT INITIAL AND
          wg_save NE 'BOLETO'.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Gerar Boleto'.
        MESSAGE i001(zsd) DISPLAY LIKE 'E' WITH TEXT-e53 TEXT-e54 TEXT-e97.
        sy-ucomm = wg_save.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7


*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
      DATA: wl_instrucoes TYPE zfi_boleto,
            vl_juros_ano  TYPE zsdt0040-juros_ano,
            vl_valor      TYPE c LENGTH 7.

      SELECT SINGLE juros_ano
        FROM zsdt0040
        INTO vl_juros_ano
         WHERE doc_simulacao EQ wg_header-doc_simulacao.

      IF sy-subrc EQ 0.
        wl_instrucoes-txt_instrucao1 = 'Sujeito a protesto.'.

        vl_valor = vl_juros_ano.
        CONCATENATE 'Após a data de vencimento será cobrado ' vl_valor ' % de Juros a.a'
                INTO wl_instrucoes-txt_instrucao2 SEPARATED BY space.
      ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
      MOVE wg_header-doc_simulacao TO wl_boleto.
      CALL FUNCTION 'Z_SD_PRINT_BOLETO'
        EXPORTING
          doc_numero     = wl_boleto
          tipo           = 'A'  "Antecipação Pagamento
          hbkid          = wg_header-hbkid "US 81799 - CBRAND  Devemos descomentar antes de publicar a US 81799 em PRD
*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
          instrucoes     = wl_instrucoes
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
        EXCEPTIONS
          nao_localizado = 1
          OTHERS         = 2.

    WHEN c_bloq.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF wg_save IS NOT INITIAL AND
          wg_save NE c_bloq.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Bloquear'.
        MESSAGE i001(zsd) DISPLAY LIKE 'E' WITH TEXT-e53 TEXT-e54 TEXT-e94.
        sy-ucomm = wg_save.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7

      PERFORM modifica_status USING 'B'.

    WHEN c_gera.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF wg_save IS NOT INITIAL AND
          wg_save NE c_bloq.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Gerar OV'.
        MESSAGE i001(zsd) DISPLAY LIKE 'E' WITH TEXT-e53 TEXT-e54 TEXT-e95.
        sy-ucomm = wg_save.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7

      PERFORM log_acao USING 'G' '' CHANGING lv_erro.

      " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
      CHECK lv_erro IS INITIAL.
      " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

      PERFORM gera_ordem_venda.

    WHEN c_sacas.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF wg_save IS NOT INITIAL AND
          wg_save NE c_sacas.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Ajustar Sacas'.
        MESSAGE i001(zsd) DISPLAY LIKE 'E' WITH TEXT-e53 TEXT-e54 TEXT-e98.
        sy-ucomm = wg_save.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7


* Início - Sara Oikawa - 38859 - Agosto/2020
*      MOVE: c_sacas TO wg_save.
      " Verifica se existe OV gerada
      DATA(count_ov) = REDUCE int4( INIT y = 0 FOR ls2 IN tg_itens WHERE ( vbeln IS NOT INITIAL ) NEXT y = y + 1 ).

      IF count_ov IS NOT INITIAL.
        " Erro : Ação não é permitida. Simulador já possui OV !
        MESSAGE i001(zsd) DISPLAY LIKE 'E' WITH TEXT-e71.
        CLEAR sy-ucomm.
      ELSE.
        MOVE: c_sacas TO wg_save.
      ENDIF.
* Fim - Sara Oikawa - 38859 - Agosto/2020

    WHEN c_dsc_abs.
      MOVE: c_dsc_abs TO wg_save.
      it_desabs = tg_itens.

*      WSB
    WHEN c_col_exp.
      IF wg_colaps EQ '@K1@'.
        wg_colaps = '@K2@'.
        dynnr_top = '0105'.
      ELSE.
        wg_colaps = '@K1@'.
        dynnr_top = '0106'.
      ENDIF.
    WHEN c_vinc_desc.
      PERFORM vinc_desc.
      PERFORM log_acao USING 'V' '' CHANGING lv_erro.

      " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
      CHECK lv_erro IS INITIAL.
      " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<


      PERFORM limpa_variavel USING sy-ucomm.
      PERFORM busca_dados_doc.
      PERFORM busca_dados.
      MOVE: c_atual TO wg_acao.

    WHEN c_show_log.
      PERFORM show_log_acao.
    WHEN c_recalc.
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077
*      CLEAR WG_SAVE.
      CLEAR wg_save.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077
      btn_fi = abap_false.
      PERFORM verifica_erros.
      PERFORM recalculo.

*Início - Sara Oikawa - 38859 - Agosto/2020
    WHEN c_altjur.

      PERFORM f_verifica_recebimentos.

      IF wg_flg_receb IS INITIAL.

        IF wg_save IS NOT INITIAL AND
           wg_save NE c_altjur.
          " Por favor, concluir a ação anterior antes de prosseguir com "Alterar Juros"
          MESSAGE i001(zsd) DISPLAY LIKE 'E' WITH TEXT-e53 TEXT-e54 TEXT-e55.
          sy-ucomm = wg_save.
          EXIT.
        ENDIF.

        MOVE c_altjur TO wg_save.

      ENDIF.

    WHEN c_altadt.

      PERFORM f_verifica_recebimentos.

      IF wg_flg_receb IS INITIAL.

        IF wg_save IS NOT INITIAL AND
           wg_save NE c_altadt.
          " Por favor, concluir a ação anterior antes de prosseguir com "Alterar Adiantamento"
          MESSAGE i001(zsd) DISPLAY LIKE 'E' WITH TEXT-e53 TEXT-e54 TEXT-e56.
          sy-ucomm = wg_save.
          EXIT.
        ENDIF.

        MOVE c_altadt TO wg_save.

      ENDIF.
*Fim - Sara Oikawa - 38859 - Agosto/2020

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
    WHEN c_altcsa.

      IF wg_save IS NOT INITIAL AND
         wg_save NE c_altcsa.
        " Por favor, concluir a ação anterior antes de prosseguir com "Alterar Cultura/Safra"
        MESSAGE i001(zsd) DISPLAY LIKE 'E' WITH TEXT-e53 TEXT-e54 TEXT-e70.
        sy-ucomm = wg_save.
        EXIT.
      ENDIF.

      MOVE c_altcsa TO wg_save.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
    WHEN c_altcpg.

      PERFORM f_verifica_recebimentos.

      IF wg_flg_receb IS INITIAL.

        IF wg_save IS NOT INITIAL AND
           wg_save NE c_altcpg.
          " Por favor, concluir a ação anterior antes de prosseguir com "Alterar Cond.Pgto"
          MESSAGE i001(zsd) DISPLAY LIKE 'E' WITH TEXT-e53 TEXT-e54 TEXT-e90.
          sy-ucomm = wg_save.
          EXIT.
        ENDIF.

        CONCATENATE TEXT-i06 TEXT-i07 INTO lv_msg.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question         = lv_msg  "
            text_button_1         = 'Sim'
            text_button_2         = 'Não'
            display_cancel_button = ' '
          IMPORTING
            answer                = lv_resp.

        IF lv_resp EQ 1.
          sy-ucomm = c_altcpg.
          wg_save  = c_altcpg.
          wg_tpsim_ant = wg_header-tpsim.
        ELSE.
          CLEAR sy-ucomm.
        ENDIF.


      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
      " Dados Compra SIGAM
    WHEN 'BTN_SG'.
      CALL SCREEN 0110 STARTING AT 4 3.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

      " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
    WHEN c_boleta_vin.
      PERFORM f_call_boleta_report.
      " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  DATA: wl_repid    TYPE sy-repid,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE,
        lt_f4       TYPE lvc_t_f4 WITH HEADER LINE.


  wl_repid = sy-repid.
  PERFORM verifica_erros.
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen    = '100'
      i_show      = space
      i_repid     = sy-repid
      i_popup     = 0
*     i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
      i_set_field = 'X_FIELD'
    IMPORTING
      e_messagem  = wg_mensagem
    TABLES
      it_msgs     = tg_msg_ret.

  IF container1 IS INITIAL.
*    INICIO WSB
    wa_layout-stylefname = 'STYLE'.
*    FIM WSB

    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = c_x.
*    WA_STABLE-ROW        = C_X.

    CREATE OBJECT container1
      EXPORTING
        container_name = 'CC_01'.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container1.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid1.

    REFRESH tl_function.

    SET HANDLER: obg_toolbar->on_toolbar FOR grid1,
                 obg_toolbar->handle_user_command FOR grid1.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    lt_f4-fieldname = 'MATNR'.
    lt_f4-register = 'X' .
    lt_f4-getbefore = 'X' .
    APPEND lt_f4 .

    PERFORM montar_layout.

    PERFORM build_dropdown.

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_itens[].

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid1->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

*    SET HANDLER: LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK FOR GRID1.
    SET HANDLER: lcl_event_handler=>on_hotspot_click         FOR grid1,
                 lcl_event_handler=>on_data_changed_finished FOR grid1,
                 lcl_event_handler=>on_data_changed          FOR grid1,
                 lcl_event_handler=>on_onf4                  FOR grid1.

  ELSE.
    PERFORM montar_layout.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

** Valida se usuário tem permissão de visuzlizar simulação
  IF wg_header-vkbur IS NOT INITIAL.
    PERFORM f_validar_esc_venda USING wg_header-vkbur
                                CHANGING sy-subrc.
    IF sy-subrc IS INITIAL.
      SELECT *
         FROM zsdt0090
         INTO TABLE tg_trans
         WHERE doc_simulacao = wg_header-doc_simulacao
         ORDER BY sequencia.

      LOOP AT tg_trans ASSIGNING FIELD-SYMBOL(<w0090>)
        WHERE estorno EQ abap_true.
        <w0090>-color = 'C600'.
      ENDLOOP.

    ENDIF.
  ENDIF.

  "Transferencias
  IF g_custom_container IS INITIAL.
    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = c_x.
    wa_layout-no_toolbar = c_x.
    wa_layout-stylefname = 'FIELD_STYLE'.
    wa_layout-info_fname = 'COLOR'.

*    WA_STABLE-ROW        = C_X.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT splitter
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = container_1.

    CREATE OBJECT grid2
      EXPORTING
        i_parent = container_1.


    PERFORM montar_layout_tra.

*    CREATE OBJECT OBG_TOOLBAR
*      EXPORTING
*        IO_ALV_GRID = GRID1.

**      * Register event handler
*    SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR GRID1.
*    SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND FOR GRID1.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
*       I_STRUCTURE_NAME     = 'ZSDT0090'\
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      CHANGING
        it_fieldcatalog      = t_fieldcatalog2[]
        it_outtab            = tg_trans[].

    SET HANDLER:
            lcl_event_handler=>on_hotspot_click_tra FOR grid2.

*    posiciona spliter na altura x
    IF tg_trans[] IS NOT INITIAL.
      CALL METHOD splitter->set_row_height
        EXPORTING
          id     = 1
          height = 100.
    ELSE.
      CALL METHOD splitter->set_row_height
        EXPORTING
          id     = 1
          height = 0.
    ENDIF.
  ELSE.
*    posiciona spliter na altura x
    IF tg_trans[] IS NOT INITIAL.
      CALL METHOD splitter->set_row_height
        EXPORTING
          id     = 1
          height = 100.
    ELSE.
      CALL METHOD splitter->set_row_height
        EXPORTING
          id     = 1
          height = 0.
    ENDIF.
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM montar_layout_infor .
  REFRESH t_fieldcat_infor.
  PERFORM montar_estrutura USING:
*       1   'ZSDT0041'  'SPART'       'TG_INFOR'  'SPART'       ' '                   '06'   ' ' ' '   ' '   ' '  ' '  ' ',
        2   'TSPAT'     'VTEXT'       'TG_INFOR'  'VTEXT'       'Descrição'           '25'   ' ' ' '   ' '   ' '  ' '  ' ',
        3   ' '         ' '           'TG_INFOR'  'VLRTOT'      'Vlr.Total'           '16'   ' ' ' '   ' '   ' '  ' '  ' '.
ENDFORM.                    "MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
*        1   'ZSDT0041'  'POSNR'         'TG_ITENS'  'POSNR'         ' '                       '04' ' '  ' '   ' '   ' '  ' '  ' ',
*        2   'ZSDT0041'  'AUART'         'TG_ITENS'  'AUART'         'TP O.V.'                 '05' ' '  ' '   ' '   ' '  ' '  ' ',
*        3   'VBAK'      'VBELN'         'TG_ITENS'  'VBELN'         'Nº O.V'                  '10' ' '  ' '   ' '   ' '  ' '  'X',
*        4   'ZSDT0041'  'SPART'         'TG_ITENS'  'SPART'         ' '                       '02' ' '  ' '   ' '   ' '  ' '  ' ',
*        5   ' '         ' '             'TG_ITENS'  'MATNR'         'Material'                '07' ' '  'X'   ' '   ' '  'X'  ' ',
*        6   'MAKT'      'MAKTX'         'TG_ITENS'  'MAKTX'         ' '                       ' '  ' '  ' '   ' '   ' '  ' '  ' ',
*        7   'ZSDT0041'  'WERKS'         'TG_ITENS'  'WERKS'         'Centro Fornec.'          '04' ' '  ' '   ' '   ' '  ' '  ' ',
**        8   'ZSDT0041'  'CHARG'         'TG_ITENS'  'CHARG'         ' '                       '05' ' '  'X'   ' '   ' '  ' '  ' ', *        **** Block campo CHARG CS2016001142>>>>>
*        8   'ZSDT0041'  'CHARG'         'TG_ITENS'  'CHARG'         ' '                       '05' ' '  ' '   ' '   ' '  ' '  ' ',
*        9   'ZSDT0041'  'INCO1'         'TG_ITENS'  'INCO1'         ' '                       '06' ' '  ' '   ' '   ' '  ' '  ' ',
*        10  'ZSDT0041'  'ZMENG'         'TG_ITENS'  'ZMENG'         ' '                       ' '  'X'  'X'   ' '   ' '  ' '  ' ',
*        11  'ZSDT0041'  'ZIEME'         'TG_ITENS'  'ZIEME'         ' '                       '04' ' '  ' '   ' '   ' '  ' '  ' ',
*        12  'ZSDT0041'  'DTVENC'        'TG_ITENS'  'DTVENC'        ' Data Vencimento'        '10' ' '  'X'   ' '   ' '  ' '  ' ',
*        13  'ZSDT0041'  'DESCONTO'      'TG_ITENS'  'DESCONTO'      '% Desconto'              '06' ' '  ' '   ' '   ' '  ' '  ' ',
*        14  ' '         ' '             'TG_ITENS'  'VL_UNIT'       'Vlr.Unit'                ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        15  'ZSDT0041'  'ZWERT'         'TG_ITENS'  'ZWERT'         'Vlr. Negociado'          ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        16  ' '         ' '             'TG_ITENS'  'CALCU'         'Calculo'                 ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        17  ' '         ' '             'TG_ITENS'  'TRUNIT'        'Troc.Unit.'              ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        18  ' '         ' '             'TG_ITENS'  'COMPR'         'Compromisso'             ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        19  'ZSDT0041'  'TRTOT'         'TG_ITENS'  'TRTOT'         'Troc.Total'              ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        20  'ZSDT0041'  'DESC_ABSOLUTO' 'TG_ITENS'  'VLR_AJUSTE'    'Vlr Ajuste'              ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        21  'ZSDT0041'  'DESC_ABSOLUTO' 'TG_ITENS'  'DESC_ABSOLUTO' 'Desc.Abs'                ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        22  'ZSDT0041'  'VLRTOT'        'TG_ITENS'  'VLRTOT'        'Vlr.Total'               ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        23  ' '         ' '             'TG_ITENS'  'MGCAD'         '% Marg. Cadastrada'      ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        24  ' '         ' '             'TG_ITENS'  'MGEFE'         '% Marg. Efetiva'         ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        25  'ZSDT0041'  'VLR_ICMS'      'TG_ITENS'  'VLR_ICMS'      'Vlr. ICMS'               ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        26  'ZSDT0041'  'VLR_COFINS'    'TG_ITENS'  'VLR_COFINS'    'Vlr. COFINS'             ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        27  'ZSDT0041'  'VLR_PIS'       'TG_ITENS'  'VLR_PIS'       'Vlr. PIS'                ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        28  'ZSDT0041'  'ZWERT_LIQDO'   'TG_ITENS'  'ZWERT_LIQDO'   'Vlr. Neg. Liqdo'         ' '  'X'  ' '   ' '   ' '  ' '  ' '.


        1   'ZSDT0041'  'POSNR'         'TG_ITENS'  'POSNR'         ' '                       '04' ' '  ' '   ' '   ' '  ' '  ' ',
        2   'ZSDT0041'  'AUART'         'TG_ITENS'  'AUART'         'TP O.V.'                 '05' ' '  ' '   ' '   ' '  ' '  ' ',
        3   'VBAK'      'VBELN'         'TG_ITENS'  'VBELN'         'Nº O.V'                  '10' ' '  ' '   ' '   ' '  ' '  'X',
        4   'ZSDT0041'  'SPART'         'TG_ITENS'  'SPART'         ' '                       '02' ' '  ' '   ' '   ' '  ' '  ' ',
        5   ' '         ' '             'TG_ITENS'  'MATNR'         'Material'                '07' ' '  'X'   ' '   ' '  'X'  ' ',
        6   'MAKT'      'MAKTX'         'TG_ITENS'  'MAKTX'         ' '                       ' '  ' '  ' '   ' '   ' '  ' '  ' ',
        7   'ZSDT0041'  'WERKS'         'TG_ITENS'  'WERKS'         'Centro Fornec.'          '04' ' '  ' '   ' '   ' '  ' '  ' ',
*        8   'ZSDT0041'  'CHARG'         'TG_ITENS'  'CHARG'         ' '                       '05' ' '  'X'   ' '   ' '  ' '  ' ', *        **** Block campo CHARG CS2016001142>>>>>
        8   'ZSDT0041'  'CHARG'         'TG_ITENS'  'CHARG'         ' '                       '05' ' '  ' '   ' '   ' '  ' '  ' ',
        9   'ZSDT0041'  'INCO1'         'TG_ITENS'  'INCO1'         ' '                       '06' ' '  ' '   ' '   ' '  ' '  ' ',

        10  'ZSDT0041'  'CULTURA_APL'   'TG_ITENS'  'CULTURA_APL'   ' '                       '02' ' '  ' '   ' '   ' '  ' '  ' ',
        11  'ZSDT0041'  'SAFRA_APL'     'TG_ITENS'  'SAFRA_APL'     ' '                       '04' ' '  ' '   ' '   ' '  ' '  ' ',

        12  'ZSDT0041'  'ZMENG'         'TG_ITENS'  'ZMENG'         ' '                       ' '  'X'  'X'   ' '   ' '  ' '  ' ',
        13  'ZSDT0041'  'ZIEME'         'TG_ITENS'  'ZIEME'         ' '                       '04' ' '  ' '   ' '   ' '  ' '  ' ',
        14  'ZSDT0041'  'DTVENC'        'TG_ITENS'  'DTVENC'        ' Data Vencimento'        '10' ' '  'X'   ' '   ' '  ' '  ' ',
        15  'ZSDT0041'  'DESCONTO'      'TG_ITENS'  'DESCONTO'      '% Desconto'              '06' ' '  ' '   ' '   ' '  ' '  ' ',
        16  ' '         ' '             'TG_ITENS'  'VL_UNIT'       'Vlr.Unit'                ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        17  'ZSDT0041'  'ZWERT'         'TG_ITENS'  'ZWERT'         'Vlr. Negociado'          ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        18  ' '         ' '             'TG_ITENS'  'CALCU'         'Calculo'                 ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        19  ' '         ' '             'TG_ITENS'  'TRUNIT'        'Troc.Unit.'              ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        20  ' '         ' '             'TG_ITENS'  'COMPR'         'Compromisso'             ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        21  'ZSDT0041'  'TRTOT'         'TG_ITENS'  'TRTOT'         'Troc.Total'              ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        22  'ZSDT0041'  'DESC_ABSOLUTO' 'TG_ITENS'  'VLR_AJUSTE'    'Vlr Ajuste'              ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        23  'ZSDT0041'  'DESC_ABSOLUTO' 'TG_ITENS'  'DESC_ABSOLUTO' 'Desc.Abs'                ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        24  'ZSDT0041'  'VLRTOT'        'TG_ITENS'  'VLRTOT'        'Vlr.Total'               ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        25  ' '         ' '             'TG_ITENS'  'MGCAD'         '% Marg. Cadastrada'      ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        26  ' '         ' '             'TG_ITENS'  'MGEFE'         '% Marg. Efetiva'         ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        27  'ZSDT0041'  'VLR_ICMS'      'TG_ITENS'  'VLR_ICMS'      'Vlr. ICMS'               ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        28  'ZSDT0041'  'VLR_COFINS'    'TG_ITENS'  'VLR_COFINS'    'Vlr. COFINS'             ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        29  'ZSDT0041'  'VLR_PIS'       'TG_ITENS'  'VLR_PIS'       'Vlr. PIS'                ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        30  'ZSDT0041'  'ZWERT_LIQDO'   'TG_ITENS'  'ZWERT_LIQDO'   'Vlr. Neg. Liqdo'         ' '  'X'  ' '   ' '   ' '  ' '  ' '.

* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

  PERFORM valida_layout TABLES t_fieldcatalog
                        USING sy-uname.

ENDFORM.                    " MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_TRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM montar_layout_tra.

  REFRESH t_fieldcatalog2.
  PERFORM montar_estrutura USING:
        01 'ZSDT0090'  'SEQUENCIA'        'TG_TRANS'   'SEQUENCIA'        '' '' '' '' '' '' '' '',
        02 'ZSDT0090'  'CATEGORIA'        'TG_TRANS'   'CATEGORIA'        'Ação' '' '' '' '' '' '' '',
        03 'ZSDT0090'  'AUART'            'TG_TRANS'   'AUART'            '' '' '' '' '' '' '' '',
        04 'ZSDT0090'  'VBELN'            'TG_TRANS'   'VBELN'            '' '' '' '' '' '' '' 'X',
        05 'ZSDT0090'  'POSNN'            'TG_TRANS'   'POSNN'            '' '' '' '' '' '' '' '',
        06 'ZSDT0090'  'SPART'            'TG_TRANS'   'SPART'            '' '' '' '' '' '' '' '',
        07 'ZSDT0090'  'ZMENG'            'TG_TRANS'   'ZMENG'            '' '' '' '' '' '' '' '',
        08 'ZSDT0090'  'ZIEME'            'TG_TRANS'   'ZIEME'            '' '' '' '' '' '' '' '',
        09 'ZSDT0090'  'NETPR'            'TG_TRANS'   'NETPR'            '' '' '' '' '' '' '' '',
        10 'ZSDT0090'  'KMEIN'            'TG_TRANS'   'KMEIN'            '' '' '' '' '' '' '' '',
        11 'ZSDT0090'  'CHARG'            'TG_TRANS'   'CHARG'            '' '' '' '' '' '' '' '',
        12 'ZSDT0090'  'MATNR'            'TG_TRANS'   'MATNR'            '' '' '' '' '' '' '' '',
        13 'ZSDT0090'  'MATKL'            'TG_TRANS'   'MATKL'            '' '' '' '' '' '' '' '',
        14 'ZSDT0090'  'INCO1'            'TG_TRANS'   'INCO1'            '' '' '' '' '' '' '' '',
        15 'ZSDT0090'  'WERKS'            'TG_TRANS'   'WERKS'            '' '' '' '' '' '' '' '',
        16 'ZSDT0090'  'KUNNR'            'TG_TRANS'   'KUNNR'            '' '' '' '' '' '' '' '',
        17 'ZSDT0090'  'AUARTV'           'TG_TRANS'   'AUARTV'           '' '' '' '' '' '' '' '',
        18 'ZSDT0090'  'VBELV'            'TG_TRANS'   'VBELV'            '' '' '' '' '' '' '' 'X',
        19 'ZSDT0090'  'POSNV'            'TG_TRANS'   'POSNV'            '' '' '' '' '' '' '' '',
        20 'ZSDT0090'  'SPARTV'           'TG_TRANS'   'SPARTV'           '' '' '' '' '' '' '' '',
        21 'ZSDT0090'  'ZMENGV'           'TG_TRANS'   'ZMENGV'           '' '' '' '' '' '' '' '',
        22 'ZSDT0090'  'ZIEMEV'           'TG_TRANS'   'ZIEMEV'           '' '' '' '' '' '' '' '',
        23 'ZSDT0090'  'NETPRV'           'TG_TRANS'   'NETPRV'           '' '' '' '' '' '' '' '',
        24 'ZSDT0090'  'KMEINV'           'TG_TRANS'   'KMEINV'           '' '' '' '' '' '' '' '',
        25 'ZSDT0090'  'CHARGV'           'TG_TRANS'   'CHARGV'           '' '' '' '' '' '' '' '',
        26 'ZSDT0090'  'MATNRV'           'TG_TRANS'   'MATNRV'           '' '' '' '' '' '' '' '',
        27 'ZSDT0090'  'MATKLV'           'TG_TRANS'   'MATKLV'           '' '' '' '' '' '' '' '',
        28 'ZSDT0090'  'INCO1V'           'TG_TRANS'   'INCO1V'           '' '' '' '' '' '' '' '',
        29 'ZSDT0090'  'WERKSV'           'TG_TRANS'   'WERKSV'           '' '' '' '' '' '' '' '',
        30 'ZSDT0090'  'KUNNRV'           'TG_TRANS'   'KUNNRV'           '' '' '' '' '' '' '' '',
        31 'ZSDT0090'  'KURRF'            'TG_TRANS'   'KURRF'            '' '' '' '' '' '' '' '',
        32 'ZSDT0090'  'VALDT'            'TG_TRANS'   'VALDT'            '' '' '' '' '' '' '' '',
        33 'ZSDT0090'  'DESC_ABSOLUTO'    'TG_TRANS'   'DESC_ABSOLUTO'    'Vlr. Desc. Absoluto' '' '' '' '' '' '' '',
*        34 'ZSDT0090'  'DESC_ABSOLUTO_LQ' 'TG_TRANS'   'DESC_ABSOLUTO_LQ' '' '' '' '' '' '' '' '',
        34 'ZSDT0090'  'USNAM'            'TG_TRANS'   'USNAM'            '' '' '' '' '' '' '' '',
        35 'ZSDT0090'  'DATA_ATUAL'       'TG_TRANS'   'DATA_ATUAL'       'Data' '' '' '' '' '' '' '',
        36 'ZSDT0090'  'HORA_ATUAL'       'TG_TRANS'   'HORA_ATUAL'       'Hora' '' '' '' '' '' '' ''.

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_opt)           TYPE c
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize)
                            VALUE(p_f4)
                            VALUE(p_hotspot).

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname        = p_field.
  w_fieldcatalog-tabname          = p_tabname.

  CASE p_field.
    WHEN 'VL_UNIT' OR 'ZWERT' OR 'CALCU'
      OR 'TRUNIT'  OR 'COMPR' OR 'TRTOT'
      OR 'VLRTOT' OR 'MGCAD'  OR 'MGEFE'.
    WHEN OTHERS.
      w_fieldcatalog-ref_table        = p_ref_tabname.
  ENDCASE.

  w_fieldcatalog-ref_field        = p_ref_fieldname.
  w_fieldcatalog-key              = ' '.

  IF wg_display IS INITIAL.
    w_fieldcatalog-edit           = p_edit.
  ENDIF.

  w_fieldcatalog-do_sum           = p_sum.
  w_fieldcatalog-col_pos          = p_col_pos.

  IF p_outputlen IS NOT INITIAL
    AND p_outputlen NE 'X'.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.

  IF p_outputlen EQ 'X'.
    w_fieldcatalog-col_opt        = p_opt.
  ENDIF.

  w_fieldcatalog-no_out = COND #( WHEN p_ref_fieldname EQ 'DESC_ABSOLUTO' THEN abap_true ELSE abap_false ).

  w_fieldcatalog-reptext          = p_scrtext_l.
  w_fieldcatalog-scrtext_s        = p_scrtext_l.
  w_fieldcatalog-scrtext_m        = p_scrtext_l.
  w_fieldcatalog-scrtext_l        = p_scrtext_l.
  w_fieldcatalog-emphasize        = p_emphasize.
  w_fieldcatalog-f4availabl       = p_f4.
  w_fieldcatalog-hotspot          = p_hotspot.

  IF p_field EQ 'VLRTOT'
  OR p_field EQ 'ZWERT'
  OR p_field EQ 'CALCU'
  OR p_field EQ 'TRUNIT'
  OR p_field EQ 'COMPR'.
    w_fieldcatalog-datatype         = 'CURR'.
  ENDIF.

*  IF P_FIELD EQ 'VLRTOT'
*  OR P_FIELD EQ 'ZWERT'
*  OR P_FIELD EQ 'CALCU'
*  OR P_FIELD EQ 'TRUNIT'
*  OR P_FIELD EQ 'COMPR'.
*    W_FIELDCATALOG-DECIMALS_O = '000002'.
*  ENDIF.

  IF p_field EQ 'AUART'.
    "W_FIELDCATALOG-DRDN_HNDL  = 1.
  ELSEIF p_field EQ 'INCO1'.
    w_fieldcatalog-drdn_hndl  = 2.
  ENDIF.

  IF p_field EQ 'MATNR'.
    w_fieldcatalog-edit_mask  = '==MATN1'.
  ENDIF.

  IF p_ref_tabname =  'ZSDT0090'.
    APPEND w_fieldcatalog TO t_fieldcatalog2.
  ELSEIF p_tabname EQ 'TG_INFOR'.
    APPEND w_fieldcatalog TO t_fieldcat_infor.
  ELSE.
    APPEND w_fieldcatalog TO t_fieldcatalog.
  ENDIF.
ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  LIMPA_VARIAVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpa_variavel USING p_acao.
  DATA: wl_doc_simulacao TYPE zsdt0040-doc_simulacao.

  CLEAR: wl_doc_simulacao.

  IF p_acao EQ c_cancel
   OR p_acao EQ c_atual.
    wl_doc_simulacao = wg_header-doc_simulacao.

    CLEAR:
           wg_header,
           wg_header_old,
           wg_0260,
           wg_area_ha,
           tg_itens,
           wg_acao,
           wg_save,
           wg_flag,
           x_field,
           tg_msg_ret,
           wg_cell,
           wg_obj,
           wg_desc_kunnr,
*Início - Sara Oikawa - 38859 - Agosto/2020
           wg_desc_emissor,
           wg_desc_ort01,
           wg_desc_regio,
           vta_sistpro,
           vlr_total,
*Fim - Sara Oikawa - 38859 - Agosto/2020
           wg_desc_vkbur,
           wg_desc_waerk,
           wg_desc_cultura,
           wg_desc_tpcult,
           wg_desc_vendedor,
           wg_desc_hbkid,
           wg_tvkot,
           wg_tvtwt,
           wg_tspat,
           style,
           wg_status,
           wg_memo,
           tg_mglobal,
           wg_desc_dat,
           s_acao,
           tg_log.

    FREE: tg_itens,
          tg_msg_ret,
          style,
          tg_itens-style,
          tg_mglobal,
          tg_trans,
          tg_log.

    wg_desc_dat = 'Dt.Pagamento'.
    wg_header-doc_simulacao = wl_doc_simulacao.
  ELSEIF p_acao EQ c_add.
    CLEAR: wg_header,
           wg_header_old,
           wg_0260,
           wg_area_ha,
           tg_itens,
           wg_acao,
           wg_save,
           x_field,
           tg_msg_ret,
           wg_desc_kunnr,
*Início - Sara Oikawa - 38859 - Agosto/2020
           wg_desc_emissor,
           wg_desc_ort01,
           wg_desc_regio,
           vta_sistpro,
           vlr_total,
*Fim - Sara Oikawa - 38859 - Agosto/2020
           wg_desc_vkbur,
           wg_desc_waerk,
           wg_desc_cultura,
           wg_desc_tpcult,
           wg_desc_vendedor,
           wg_desc_hbkid,
           wg_status,
           wg_memo,
           tg_mglobal,
           wg_desc_dat,
           s_acao,
           tg_log.

    REFRESH: tg_itens,
             tg_msg_ret,
             style,
             tg_itens-style,
             tg_mglobal,
             tg_log.

    wg_desc_dat = 'Dt.Pagamento'.
    wg_header-antec = 12.
*    IF     WG_HEADER-ANTEC = 0.
*      WG_HEADER-ANTEC = 12.
*    ENDIF.

  ELSEIF p_acao EQ c_tpord.
    CLEAR: tg_itens.
*           WG_HEADER-PREC_CULT,
*           WG_HEADER-JUROS_ANO,
*           WG_HEADER-VLR_ADTO,
*           WG_HEADER-ADTO_HA,
*           WG_HEADER-AREA_PENHOR,
*           WG_HEADER-TROTOTSC,
*           WG_HEADER-SCHA,
*           WG_HEADER-VLRTOT,
*           WG_HEADER-COMPRSC.
*           WG_HEADER-DTPGTCULT.

*    REFRESH: TG_ITENS, STYLE, TG_ITENS-STYLE.
    LOOP AT tg_itens.
      CLEAR: "tg_itens-zwert, "BUG 53832
*             TG_ITENS-DTVENC,
             "tg_itens-desconto, "BUG 53832
             tg_itens-calcu,
             tg_itens-trunit,
             "tg_itens-siumb,    "BUG 53832
             tg_itens-trtot,
             tg_itens-compr,
             tg_itens-mgcad,
             tg_itens-mgefe.
* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
** Na alteração de CondPagto, não VBELN se estiver preenchido
*             TG_ITENS-VLRTOT,
*             TG_ITENS-VBELN.
      "tg_itens-vlrtot.  "BUG 53832
*      IF tg_itens-vbeln IS NOT INITIAL AND wg_save NE c_altcpg.
*        CLEAR tg_itens-vbeln.
*      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

      MODIFY tg_itens.
    ENDLOOP.
    PERFORM calcula_itens.
  ELSE.
    CLEAR: tg_itens, s_acao.
    REFRESH: tg_itens, tg_itens-style.
  ENDIF.


ENDFORM.                    " LIMPA_VARIAVEL
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen OUTPUT.
  " Bloco de Código abaixo movido para SubScreen 0103 - PBO MODIFY_SCREEN_0103
*  DATA: VALUES       TYPE VRM_VALUES WITH HEADER LINE.
*  FIELD-SYMBOLS: <FS_CAMPO> TYPE ANY.
*  DATA: IT_VBFA TYPE TABLE OF VBFA,
*        WA_VBFA TYPE VBFA,
*        IT_VBAP TYPE TABLE OF VBAP,
*        WA_VBAP TYPE VBAP,
*        IT_VBAK TYPE TABLE OF VBAK,
*        WA_VBAK TYPE VBAK,
*        IT_41 TYPE TABLE OF ZSDT0041,
*        WA_41 TYPE ZSDT0041.
*
*  IF INIT IS INITIAL.
*    SELECT *
*      FROM SETLEAF
*      INTO TABLE TG_SETLEAF_CULT
*       WHERE SETNAME EQ 'MAGGI_ZSDT0044_03'.
*
*    IF SY-SUBRC IS INITIAL.
*      SELECT *
*        FROM SETLINET
*        INTO TABLE TG_SETLINET_CULT
*        FOR ALL ENTRIES IN TG_SETLEAF_CULT
*         WHERE SETNAME EQ TG_SETLEAF_CULT-SETNAME
*           AND LANGU   EQ SY-LANGU
*           AND LINEID  EQ TG_SETLEAF_CULT-LINEID.
*    ENDIF.
*    LOOP AT TG_SETLEAF_CULT.
*      READ TABLE TG_SETLINET_CULT
*        WITH KEY SETNAME = TG_SETLEAF_CULT-SETNAME
*                 LINEID  = TG_SETLEAF_CULT-LINEID.
*      IF SY-SUBRC IS INITIAL.
*
*        VALUES-TEXT = TG_SETLINET_CULT-DESCRIPT.
*        VALUES-KEY  = TG_SETLEAF_CULT-VALFROM.
*        APPEND VALUES.
*      ENDIF.
*    ENDLOOP.
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        ID              = 'WG_HEADER-TPCULT'
*        VALUES          = VALUES[]
*      EXCEPTIONS
*        ID_ILLEGAL_NAME = 1
*        OTHERS          = 2.
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*    REFRESH: VALUES.
*    CLEAR: VALUES.
*
*    VALUES-TEXT = 'CIF'.
*    VALUES-KEY  = 'CIF'.
*    APPEND VALUES.
*
*    VALUES-TEXT = 'FOB'.
*    VALUES-KEY  = 'FOB'.
*    APPEND VALUES.
*
*    VALUES-TEXT = 'CPT'.
*    VALUES-KEY  = 'CPT'.
*    APPEND VALUES.
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        ID              = 'WG_HEADER-FCULT'
*        VALUES          = VALUES[]
*      EXCEPTIONS
*        ID_ILLEGAL_NAME = 1
*        OTHERS          = 2.
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
*
*    ENDIF.
*
*
*    REFRESH VALUES.
*    VALUES-TEXT = 'Adiantamento'.
*    VALUES-KEY  = 'AD'.
*    APPEND VALUES.
*
*    VALUES-TEXT = 'Venda A Vista'.
*    VALUES-KEY  = 'VV'.
*    APPEND VALUES.
*
*    VALUES-TEXT = 'Venda A Prazo'.
*    VALUES-KEY  = 'VP'.
*    APPEND VALUES.
*
*    VALUES-TEXT = 'Troca Safra'.
*    VALUES-KEY  = 'TS'.
*    APPEND VALUES.
*
*    VALUES-TEXT = 'Troca a Vista'.
*    VALUES-KEY  = 'TV'.
*    APPEND VALUES.
*
*    " Usuários que podem ter acesso as condições de pagamento adicionais
*    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*      EXPORTING
*        CLASS         = '0000'
*        SETNR         = 'MAGGI_ZSDT0044_04'
*      TABLES
*        SET_VALUES    = T_USERMD
*      EXCEPTIONS
*        SET_NOT_FOUND = 1
*        OTHERS        = 2.
*    IF SY-SUBRC <> 0.
**     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*    SORT T_USERMD BY FROM.
*    READ TABLE T_USERMD WITH KEY FROM = SY-UNAME.
*    IF SY-SUBRC = 0.
*      VALUES-TEXT = 'Venda Futura'.
*      VALUES-KEY  = 'VF'.
*      APPEND VALUES.
*
*      VALUES-TEXT = 'Bonificação'.
*      VALUES-KEY  = 'BN'.
*      APPEND VALUES.
*    ENDIF.
*
*
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        ID              = 'WG_HEADER-TPSIM'
*        VALUES          = VALUES[]
*      EXCEPTIONS
*        ID_ILLEGAL_NAME = 1
*        OTHERS          = 2.
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
*
*    ENDIF.
*
*    INIT = C_X.
*  ENDIF.
*
*** Adiantamento
*  IF WG_HEADER-TPSIM(1) EQ C_A.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 EQ C_TRO
*      OR SCREEN-GROUP1 EQ C_VIS.
*        IF SCREEN-NAME NE 'WG_HEADER-VLRTOT'
*        OR SCREEN-NAME NE 'WG_HEADER-TPCULT'
*        OR SCREEN-NAME NE 'WG_HEADER-FCULT'
*        OR SCREEN-NAME NE 'WG_HEADER-DTENT'.
*          ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*          IF <FS_CAMPO> IS ASSIGNED.
*            CLEAR: <FS_CAMPO>.
*          ENDIF.
*          UNASSIGN <FS_CAMPO>.
*          SCREEN-ACTIVE = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDIF.
*
*      IF SCREEN-NAME = 'WG_HEADER-ANTEC'
*      OR SCREEN-NAME = 'WG_HEADER-DTINIJUROS'
*      OR SCREEN-NAME = 'WG_HEADER-KURSF'.
*        ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*        IF <FS_CAMPO> IS ASSIGNED.
*          CLEAR: <FS_CAMPO>.
*        ENDIF.
*        UNASSIGN <FS_CAMPO>.
*
*        SCREEN-ACTIVE = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*    CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
*      IMPORTING
*        ET_FIELDCATALOG = T_FIELDCATALOG.
*
*    LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
*      IF W_FIELDCATALOG-FIELDNAME EQ 'CALCU'
*      OR W_FIELDCATALOG-FIELDNAME EQ 'TRUNIT'
*      OR W_FIELDCATALOG-FIELDNAME EQ 'TRTOT'.
*
*        W_FIELDCATALOG-NO_OUT = C_X.
*      ELSE.
*        W_FIELDCATALOG-NO_OUT = SPACE.
*
*      ENDIF.
*
*      MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
*      CLEAR: W_FIELDCATALOG.
*    ENDLOOP.
*
*    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
*      EXPORTING
*        IT_FIELDCATALOG = T_FIELDCATALOG.
*** Troca
*  ELSEIF WG_HEADER-TPSIM(1) EQ C_T.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 EQ C_ADT
*      OR SCREEN-GROUP1 EQ C_VIS.
*        IF SCREEN-NAME NE 'WG_HEADER-VLRTOT'.
*          IF SCREEN-NAME NE 'WG_DESC_DAT'.
*            ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*            IF <FS_CAMPO> IS ASSIGNED.
*              CLEAR: <FS_CAMPO>.
*            ENDIF.
*            UNASSIGN <FS_CAMPO>.
*          ENDIF.
*          SCREEN-ACTIVE = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDIF.
*
*      IF SCREEN-NAME = 'WG_HEADER-KURSF'.
*        ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*        IF <FS_CAMPO> IS ASSIGNED.
*          CLEAR: <FS_CAMPO>.
*        ENDIF.
*        UNASSIGN <FS_CAMPO>.
*        SCREEN-ACTIVE = 0.
*        MODIFY SCREEN.
*      ENDIF.
*
*      IF SCREEN-NAME = 'WG_HEADER-ANTEC'.
*        IF WG_HEADER-TPSIM+1(1) EQ 'V'.
*          IF SCREEN-NAME NE 'WG_DESC_DAT'.
*            ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*            IF <FS_CAMPO> IS ASSIGNED.
*              CLEAR: <FS_CAMPO>.
*            ENDIF.
*            UNASSIGN <FS_CAMPO>.
*          ENDIF.
*          SCREEN-ACTIVE = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*    CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
*      IMPORTING
*        ET_FIELDCATALOG = T_FIELDCATALOG.
*
*    LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
*      IF W_FIELDCATALOG-FIELDNAME EQ 'COMPR'.
*
*        W_FIELDCATALOG-NO_OUT = C_X.
*      ELSE.
*        W_FIELDCATALOG-NO_OUT = SPACE.
*      ENDIF.
*      MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
*      CLEAR: W_FIELDCATALOG.
*    ENDLOOP.
*
*    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
*      EXPORTING
*        IT_FIELDCATALOG = T_FIELDCATALOG.
*** A Vista
*  ELSEIF WG_HEADER-TPSIM(1) EQ C_V.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 EQ C_ADT
*      OR SCREEN-GROUP1 EQ C_TRO.
*        IF SCREEN-NAME NE 'WG_HEADER-VLRTOT'.
*          IF SCREEN-NAME NE 'WG_DESC_DAT'.
*            IF ( SCREEN-NAME EQ 'WG_HEADER-JUROS_ANO'  AND WG_HEADER-TPSIM EQ 'VP') OR
*               ( SCREEN-NAME EQ 'WG_HEADER-DTINIJUROS' AND WG_HEADER-TPSIM EQ 'VP').
*            ELSE.
*              ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*              IF <FS_CAMPO> IS ASSIGNED.
*                CLEAR: <FS_CAMPO>.
*              ENDIF.
*              UNASSIGN <FS_CAMPO>.
*            ENDIF.
*          ENDIF.
*          SCREEN-ACTIVE = 0.
*          IF ( SCREEN-NAME EQ 'WG_HEADER-JUROS_ANO' AND WG_HEADER-TPSIM EQ 'VP').
*            WG_DESC_DAT = 'Data Vencimento'.
*            SCREEN-ACTIVE = 1.
*          ENDIF.
*          IF ( SCREEN-NAME EQ 'WG_HEADER-DTINIJUROS' AND WG_HEADER-TPSIM EQ 'VP').
*            SCREEN-ACTIVE = 1.
*          ENDIF.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDIF.
*
*      IF ( SCREEN-NAME = 'WG_HEADER-DTPGTCULT' OR SCREEN-NAME = 'WG_DESC_DAT') AND ( WG_HEADER-TPSIM NE 'VV' AND WG_HEADER-TPSIM NE 'VP' ).
*        IF SCREEN-NAME NE 'WG_DESC_DAT'.
*          ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*          IF <FS_CAMPO> IS ASSIGNED.
*            CLEAR: <FS_CAMPO>.
*          ENDIF.
*          UNASSIGN <FS_CAMPO>.
*        ENDIF.
*        SCREEN-ACTIVE   = 0.
*        MODIFY SCREEN.
*      ENDIF.
*
*      IF SCREEN-NAME = 'WG_HEADER-KURSF' AND WG_HEADER-TPSIM NE 'VF'.
*        ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*        IF <FS_CAMPO> IS ASSIGNED.
*          CLEAR: <FS_CAMPO>.
*        ENDIF.
*        UNASSIGN <FS_CAMPO>.
*        SCREEN-ACTIVE = 0.
*        MODIFY SCREEN.
*      ENDIF.
*      IF SCREEN-NAME = 'WG_HEADER-ANTEC'.
*        IF SCREEN-NAME NE 'WG_DESC_DAT'.
*          ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*          IF <FS_CAMPO> IS ASSIGNED.
*            CLEAR: <FS_CAMPO>.
*          ENDIF.
*          UNASSIGN <FS_CAMPO>.
*        ENDIF.
*        SCREEN-ACTIVE = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*
*    CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
*      IMPORTING
*        ET_FIELDCATALOG = T_FIELDCATALOG.
*
*
*    LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
*      IF W_FIELDCATALOG-FIELDNAME EQ 'COMPR'
*      OR W_FIELDCATALOG-FIELDNAME EQ 'TRUNIT'
*      OR W_FIELDCATALOG-FIELDNAME EQ 'SIUMB'
*      OR W_FIELDCATALOG-FIELDNAME EQ 'TRTOT'
*      OR W_FIELDCATALOG-FIELDNAME EQ 'CALCU'.
*
*        W_FIELDCATALOG-NO_OUT = C_X.
*      ELSE.
*        W_FIELDCATALOG-NO_OUT = SPACE.
*      ENDIF.
*      MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
*      CLEAR: W_FIELDCATALOG.
*    ENDLOOP.
*
*    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
*      EXPORTING
*        IT_FIELDCATALOG = T_FIELDCATALOG.
*  ELSE.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 EQ C_ADT
*      OR SCREEN-GROUP1 EQ C_TRO
*      OR SCREEN-GROUP1 EQ C_VIS.
*        IF  SCREEN-NAME EQ 'WG_HEADER-VLRTOT' AND WG_HEADER-TPSIM EQ 'BN'.
*          SCREEN-ACTIVE = 1.
*        ELSE.
*          SCREEN-ACTIVE = 0.
*        ENDIF.
*        MODIFY SCREEN.
*      ENDIF.
*      IF ( SCREEN-NAME = 'WG_HEADER-DTPGTCULT' OR SCREEN-NAME = 'WG_DESC_DAT' OR SCREEN-NAME = 'WG_HEADER-ANTEC' ) AND WG_HEADER-TPSIM EQ 'BN'.
*        IF SCREEN-NAME NE 'WG_DESC_DAT'.
*          ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*          IF <FS_CAMPO> IS ASSIGNED.
*            CLEAR: <FS_CAMPO>.
*          ENDIF.
*          UNASSIGN <FS_CAMPO>.
*        ENDIF.
*        SCREEN-ACTIVE = 0.
*        MODIFY SCREEN.
*      ENDIF.
*
*      IF SCREEN-NAME = 'WG_HEADER-KURSF' AND WG_HEADER-TPSIM EQ 'BN'.
*        ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*        IF <FS_CAMPO> IS ASSIGNED.
*          CLEAR: <FS_CAMPO>.
*        ENDIF.
*        UNASSIGN <FS_CAMPO>.
*        SCREEN-ACTIVE = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*
*  ENDIF.
*
*  IF WG_ACAO EQ C_ADD
*  OR WG_ACAO EQ C_MODIF.
*    LOOP AT TG_ITENS WHERE VBELN IS NOT INITIAL.
*
*    ENDLOOP.
*    LOOP AT SCREEN.
*      IF SY-SUBRC IS NOT INITIAL.
*        IF SCREEN-GROUP2 EQ 'A1'.
*          IF SCREEN-NAME EQ 'PB_DESCP'.
*            SCREEN-INVISIBLE = 0.
*          ENDIF.
*          SCREEN-INPUT = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDIF.
*      IF SCREEN-GROUP2 EQ 'A2'.
*        SCREEN-INPUT = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*
**** modificar aqui
*    LOOP AT TG_ITENS WHERE VBELN IS NOT INITIAL.
*
*    ENDLOOP.
*    IF SY-SUBRC IS INITIAL.
*      IF GRID1 IS NOT INITIAL.
*
*        CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
*          IMPORTING
*            ET_FIELDCATALOG = T_FIELDCATALOG.
*
**      INSERT LINES OF STYLE INTO TABLE TG_ITENS-STYLE.
****         FIM DE BLOQUEI DE CAMPOS
*
*        LOOP AT TG_ITENS.
*          REFRESH: TG_ITENS-STYLE.
*          REFRESH: STYLE.
*          CLEAR: WA_STYLE.
*          LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG
*           WHERE EDIT EQ C_X.
*            WA_STYLE-FIELDNAME = W_FIELDCATALOG-FIELDNAME.
*            IF TG_ITENS-VBELN IS NOT INITIAL.
*              WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*            ELSE.
*              WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*            ENDIF.
*            INSERT  WA_STYLE INTO TABLE STYLE .
*          ENDLOOP.
*          INSERT LINES OF STYLE INTO TABLE TG_ITENS-STYLE.
*          MODIFY TG_ITENS.
*        ENDLOOP.
*
*      ENDIF.
*    ENDIF.
*
*  ELSEIF WG_ACAO EQ C_DESCONT.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP2 EQ 'A2'.
*        SCREEN-INPUT = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*
*  CASE SY-UCOMM.
*      "WSB SCREEN SACA
*    WHEN C_SACAS.
*      LOOP AT SCREEN.
*        IF  SCREEN-NAME EQ 'WG_HEADER-TROTOTSC'
*         OR SCREEN-NAME EQ 'CONVERT_TRATOTSC'.
*
*          SCREEN-INPUT = 1.
*          MODIFY SCREEN.
*
*        ENDIF.
*      ENDLOOP.
*
*    WHEN C_DSC_ABS.
*
*      LOOP AT TG_ITENS.
*        REFRESH: IT_VBFA, IT_VBAP, IT_41, IT_VBAK.
*
*        SELECT *
*          FROM VBFA
*          INTO TABLE IT_VBFA
*          WHERE VBELV EQ TG_ITENS-VBELN.
*
*        IF IT_VBFA IS NOT INITIAL.
*          REFRESH IT_VBFA.
*
*          SELECT *
*            FROM VBAK
*            INTO TABLE IT_VBAK
*            WHERE VBELN EQ TG_ITENS-VBELN.
*
*          IF IT_VBAK IS NOT INITIAL.
*            SELECT *
*              FROM VBAP
*              INTO TABLE IT_VBAP
*              FOR ALL ENTRIES IN IT_VBAK
*              WHERE VBELN EQ IT_VBAK-VBELN.
*
*            IF IT_VBAP IS NOT INITIAL.
*              SELECT *
*                FROM VBFA
*                INTO TABLE IT_VBFA
*                FOR ALL ENTRIES IN IT_VBAP
*                WHERE VBELV EQ IT_VBAP-VBELN.
*
*              LOOP AT IT_VBAP INTO WA_VBAP.
*                READ TABLE IT_VBFA TRANSPORTING NO FIELDS WITH KEY VBELV = WA_VBAP-VBELN
*                                                                   POSNV = WA_VBAP-POSNR.
*                IF SY-SUBRC IS INITIAL.
*                  SELECT *
*                  FROM ZSDT0041
*                  APPENDING TABLE IT_41
*                    WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO
*                      AND POSNR EQ TG_ITENS-POSNR
*                      AND MATNR EQ WA_VBAP-MATNR.
*
*                ENDIF.
*              ENDLOOP.
*            ENDIF.
*          ENDIF.
*        ENDIF.
**        IF IT_41 IS NOT INITIAL.
*
*        CLEAR: WA_STYLE.
*        REFRESH: TG_ITENS-STYLE.
*        WA_STYLE-FIELDNAME = 'DESC_ABSOLUTO'.
*
**          IF TG_ITENS-VBELN IS NOT INITIAL.
*        IF IT_41 IS NOT INITIAL.
*          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
**          ENDIF.
*        ELSE.
*          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*        ENDIF.
*
*
*        READ TABLE STYLE TRANSPORTING NO FIELDS WITH KEY FIELDNAME = 'DESC_ABSOLUTO'.
*        IF SY-SUBRC IS INITIAL.
*          DELETE STYLE WHERE FIELDNAME = 'DESC_ABSOLUTO'.
*          INSERT  WA_STYLE INTO TABLE STYLE.
*        ELSE.
*          INSERT  WA_STYLE INTO TABLE STYLE.
*        ENDIF.
*
**
**        WA_STYLE-FIELDNAME = 'VBELN'.
**        WA_STYLE-STYLE = '00000006'.
**
**        INSERT  WA_STYLE INTO TABLE STYLE .
**        ELSE.
*
**          CLEAR: WA_STYLE.
**          REFRESH: TG_ITENS-STYLE.
*
**          WA_STYLE-FIELDNAME = 'DESC_ABSOLUTO'.
*
**          IF TG_ITENS-VBELN IS NOT INITIAL.
**            WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
**          ENDIF.
*
**          INSERT  WA_STYLE INTO TABLE STYLE .
*
**        ENDIF.
*        INSERT LINES OF STYLE INTO TABLE TG_ITENS-STYLE.
*        MODIFY TG_ITENS.
*
*      ENDLOOP.
*
*  ENDCASE.
  " Bloco de Código acima movido para SubScreen 0103 - PBO MODIFY_SCREEN_0103

  PERFORM valida_layout TABLES t_fieldcatalog
                        USING sy-uname.

  CALL METHOD grid1->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  PERFORM verifica_erros.
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen    = '100'
      i_show      = space
      i_repid     = sy-repid
      i_popup     = 0
*     i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
      i_set_field = 'X_FIELD'
    IMPORTING
      e_messagem  = wg_mensagem
    TABLES
      it_msgs     = tg_msg_ret.

  IF x_field IS NOT INITIAL.
    SET CURSOR FIELD x_field."'WG_DESC_OPERACAO'.
  ENDIF.

  IF wg_cell IS NOT INITIAL .
    REFRESH: tg_cell.
    APPEND wg_cell TO tg_cell.
    CALL METHOD grid1->set_selected_cells
      EXPORTING
        it_cells = tg_cell[].
  ENDIF.

ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_DOC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_doc INPUT.
  DATA: BEGIN OF tl_doc OCCURS 0,
          doc_simulacao TYPE zsdt0040-doc_simulacao,
          name1         TYPE kna1-name1,
          safra         TYPE zsdt0040-safra,
          vkbur         TYPE zsdt0040-vkbur,
          tpsim         TYPE zsded007,
          status        TYPE zsdt0040-status,
          erdat         TYPE zsdt0040-erdat,
        END OF tl_doc.
  DATA: tl_0040       TYPE TABLE OF zsdt0040 WITH HEADER LINE,
        tl_kna1       TYPE TABLE OF kna1 WITH HEADER LINE,

*        TL_VALUES     TYPE VRM_VALUES,
*        WL_VALUES     TYPE LINE OF VRM_VALUES,

        tl_set        TYPE TABLE OF rgsb4      WITH HEADER LINE,

        tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  REFRESH: tl_doc, tl_0040, tl_kna1.
  CLEAR: tl_doc, tl_0040, tl_kna1.

  SELECT *
    FROM zsdt0040
    INTO TABLE tl_0040.

  IF sy-subrc IS INITIAL.
    SELECT *
      FROM kna1
      INTO TABLE tl_kna1
       FOR ALL ENTRIES IN tl_0040
     WHERE kunnr EQ tl_0040-kunnr.
  ENDIF.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class           = '0000'
      setnr           = 'MAGGI_ZSDT0044_05'
      no_descriptions = space
      no_rw_info      = space
    TABLES
      set_values      = tl_set
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

***  Busca texto de tipo de operação
*  CALL FUNCTION 'FICO_DOMAIN_VALUES_GET'
*    EXPORTING
*      I_TABLE_NAME = 'ZSDT0040'
*      I_FIELD_NAME = 'TPSIM'
*    IMPORTING
*      E_T_LIST     = TL_VALUES.

  LOOP AT tl_0040.
    READ TABLE tl_kna1
      WITH KEY kunnr = tl_0040-kunnr.

*    READ TABLE TL_VALUES INTO WL_VALUES WITH KEY KEY = TL_0040-TPSIM.
    READ TABLE tl_set WITH KEY from = tl_0040-tpsim.
    MOVE: tl_0040-doc_simulacao TO tl_doc-doc_simulacao,
          tl_0040-safra         TO tl_doc-safra,
          tl_0040-vkbur         TO tl_doc-vkbur,
          tl_kna1-name1         TO tl_doc-name1,
*          WL_VALUES-TEXT        TO TL_DOC-TPSIM,
          tl_set-title          TO tl_doc-tpsim,
          tl_0040-status        TO tl_doc-status,
          tl_0040-erdat         TO tl_doc-erdat.

    APPEND tl_doc.
    CLEAR: tl_doc, tl_kna1.

  ENDLOOP.

  SORT: tl_doc BY erdat DESCENDING.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'DOC_SIMULACAO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'WG_HEADER-DOC_SIMULACAO'
      value_org       = 'S'
    TABLES
      value_tab       = tl_doc
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

ENDMODULE.                 " SEARCH_DOC  INPUT
*&---------------------------------------------------------------------*
*&      Form  get_next_number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_7995   text
*      -->P_7996   text
*      <--P_VL_NRONC  text
*----------------------------------------------------------------------*
FORM get_next_number  USING    p_object   "TYPE nrobj
                               p_nr_range "TYPE nrnr
                      CHANGING p_number.

  CLEAR p_number.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = p_nr_range
      object                  = p_object
    IMPORTING
      number                  = p_number
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc NE 0.
    CLEAR: p_number.
    MESSAGE e836(sd) WITH 'O intervalo de numeração,'
                      'não foi encontrado!'.
  ELSE.
    wg_flag = c_x.
  ENDIF.

ENDFORM.                    " get_next_number
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_dados_doc.

  DATA: wl_0040 TYPE zsdt0040,
        tl_0041 TYPE TABLE OF zsdt0041,
        tl_0186 TYPE TABLE OF zsdt0186 WITH HEADER LINE,
        tl_makt TYPE TABLE OF makt.

  FREE wa_t001k.

  SELECT SINGLE *
    FROM zsdt0040
    INTO wl_0040
     WHERE doc_simulacao EQ wg_header-doc_simulacao.

  tpsim = wl_0040-tpsim.

  IF sy-subrc IS INITIAL.

    PERFORM f_validar_usuario USING tpsim CHANGING sy-subrc.
    IF sy-subrc IS INITIAL.
** Valida se usuário tem permissão de visuzlizar simulação
      PERFORM f_validar_esc_venda USING wl_0040-vkbur
                                  CHANGING sy-subrc.
      IF sy-subrc IS INITIAL.

        SELECT *
          FROM zsdt0041
          INTO TABLE tl_0041
           WHERE doc_simulacao EQ wg_header-doc_simulacao.

        IF sy-subrc IS INITIAL.
          SELECT *
            FROM makt
            INTO TABLE tl_makt
            FOR ALL ENTRIES IN tl_0041
             WHERE matnr EQ tl_0041-matnr
               AND spras EQ sy-langu.

        ENDIF.

        SELECT SINGLE *
          FROM t001k
            INTO wa_t001k
                WHERE bwkey EQ wg_header-vkbur.

        SELECT *
          FROM zsdt0186
          INTO TABLE tl_0186
            WHERE doc_simulacao EQ wg_header-doc_simulacao.

*Início - Sara Oikawa - 38859 - Agosto/2020
        IF sy-ucomm EQ c_copy.
          REFRESH tl_0186.
        ENDIF.
*Fim - Sara Oikawa - 38859 - Agosto/2020


* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
        " Busca Vínculo Dados de Compra SIGAM
        IF sy-ucomm EQ c_atual.
          SELECT SINGLE *
             FROM zsdt0260
             INTO wg_0260
            WHERE doc_simulacao EQ wg_header-doc_simulacao.
        ENDIF.

* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

        PERFORM monta_dados_doc TABLES: tl_0041
                                        tl_makt
                                        tl_0186
                                 USING  wl_0040.
      ENDIF.

    ELSE.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Usuário não possui acesso as '
                                             'Condições de Pag. Bonificação/Venda Futura/Permuta!'.
    ENDIF.

  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'O documento de simulação de venda'
                                           'não foi encontrado'.
  ENDIF.



ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_USUARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM f_validar_usuario USING  p_tpsim TYPE char2 CHANGING c_erro  TYPE sy-subrc .


  CASE p_tpsim.
    WHEN 'BN' OR 'CF' OR 'PM'.

      CLEAR c_erro.

      CALL FUNCTION 'G_SET_GET_ALL_VALUES'
        EXPORTING
          class         = '0000'
          setnr         = 'MAGGI_ZSDT0044_04'
        TABLES
          set_values    = t_usermd
        EXCEPTIONS
          set_not_found = 1
          OTHERS        = 2.

      IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      SORT t_usermd BY from.
      READ TABLE t_usermd WITH KEY from = sy-uname.

      c_erro = sy-subrc.

    WHEN OTHERS.
      c_erro = 0.
  ENDCASE.


ENDFORM.                    " F_VALIDAR_USUARIO

*&---------------------------------------------------------------------*
*&      Form  MONTA_DADOS_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_0041  text
*      -->P_WL_0040  text
*----------------------------------------------------------------------*
FORM monta_dados_doc  TABLES   tl_0041 STRUCTURE zsdt0041
                               tl_makt STRUCTURE makt
                               tl_0186 STRUCTURE zsdt0186
                      USING    wl_0040 TYPE zsdt0040.


  DATA: wl_ov_gerada,
        wl_0037      TYPE zsdt0037,
        wl_mara      TYPE mara.

  CLEAR: wg_header, wg_header_old,tg_itens, wl_ov_gerada, wl_0037.
  REFRESH: tg_itens.

  MOVE: wl_0040-doc_simulacao  TO wg_header-doc_simulacao,
        wl_0040-kunnr          TO wg_header-kunnr,
        wl_0040-vkbur          TO wg_header-vkbur,
        wl_0040-fazenda        TO wg_header-fazenda,
        wl_0040-tpsim          TO wg_header-tpsim,
        wl_0040-vendedor       TO wg_header-vendedor,
        wl_0040-area_ha        TO wg_area_ha,
        wl_0040-waerk          TO wg_header-waerk,
        wl_0040-cultura        TO wg_header-cultura,
        wl_0040-safra          TO wg_header-safra,
        wl_0040-ernam          TO wg_header-ernam,
        wl_0040-erdat          TO wg_header-erdat,
        wl_0040-erzet          TO wg_header-erzet,
        wl_0040-prec_ant_cult  TO wg_header-prec_ant_cult,
        wl_0040-juros_ano      TO wg_header-juros_ano,
        wl_0040-vlr_adto       TO wg_header-vlr_adto,
        wl_0040-adto_ha        TO wg_header-adto_ha,
        wl_0040-area_penhor    TO wg_header-area_penhor,
        wl_0040-vkorg          TO wg_header-vkorg,
        wl_0040-meio_pago      TO wg_header-meio_pago,
*Início - Sara Oikawa - 38859 - Agosto/2020
        wl_0040-meio_pago      TO wg_meio_pago,
*Fim - Sara Oikawa - 38859 - Agosto/2020
        wl_0040-vtweg          TO wg_header-vtweg,

* Criado o CONVERT_TRATOTSC para remover as virgulas na impressão na Tela.
*        WL_0040-TROTOTSC      TO WG_HEADER-TROTOTSC,
        wl_0040-trototsc       TO convert_tratotsc,
        convert_tratotsc       TO wg_header-trototsc,
        wl_0040-scha           TO wg_header-scha,
        wl_0040-vlrtot         TO wg_header-vlrtot,
        wl_0040-comprsc        TO wg_header-comprsc,
        wl_0040-tpcult         TO wg_header-tpcult,
        wl_0040-fcult          TO wg_header-fcult,
        wl_0040-dtent          TO wg_header-dtent,
        wl_0040-antec          TO wg_header-antec,
        wl_0040-dtpgtcult      TO wg_header-dtpgtcult,

        " 09.01.2023 - RAMON - 61181 -->
        wl_0040-dtpgtcult      TO wg_header-dtprevpgto,
        " 09.01.2023 - RAMON - 61181 --<

        wl_0040-dtinijuros     TO wg_header-dtinijuros,
        wl_0040-kursf          TO wg_header-kursf,

        wl_0040-taxa_frete     TO wg_header-taxa_frete,

        wl_0040-dt_entrega_sem TO wg_header-dt_entrega_sem,
        wl_0040-dt_entrega_def TO wg_header-dt_entrega_def,
        wl_0040-dt_entrega_fet TO wg_header-dt_entrega_fet,
        wl_0040-meio_pago      TO wg_header-meio_pago,
*Início - Sara Oikawa - 38859 - Agosto/2020
        wl_0040-meio_pago      TO wg_meio_pago,
*Fim - Sara Oikawa - 38859 - Agosto/2020
        wl_0040-dtvencov       TO wg_header-dtvencov,
        wl_0040-repique        TO wg_header-repique,
        wl_0040-funrural       TO wg_header-funrural,

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
        wl_0040-inss               TO wg_header-inss,
        wl_0040-facs               TO wg_header-facs,
        wl_0040-funrural           TO wg_header-funrural,
        wl_0040-funuser            TO wg_header-funuser,
        wl_0040-fundata            TO wg_header-fundata,
        wl_0040-funhora            TO wg_header-funhora,
        wl_0040-hbkid              TO wg_header-hbkid, " US 81799 - CBRAND

  " 21.02.2023 - RAMON - 102323 -->
  wl_0040-ecommerce  TO wg_header-ecommerce,

*#141572 -  ITSOUZA - Inicio
        wl_0040-fundeinfra         TO wg_header-fundeinfra      ,
        wl_0040-fundeinfra_exce    TO wg_header-fundeinfra_exce .
*#141572 -  ITSOUZA - Fim

  IF sy-ucomm NE 'COPY'.
    MOVE wl_0040-id_order_ecommerce TO wg_header-id_order_ecommerce.
  ENDIF.

  " 21.02.2023 - RAMON - 102323 --<


  CASE wl_0040-status.
    WHEN ''.  wg_status = icon_initial.
    WHEN c_a. wg_status = icon_release.
    WHEN c_r. wg_status = icon_defect.
    WHEN c_b. wg_status = icon_gis_pan.
  ENDCASE.

*-CS2019001753-04.01.2023-#65741-JT-inicio
  SELECT SINGLE *
    FROM t001k
    INTO wa_t001k
   WHERE bwkey EQ wg_header-vkbur.
*-CS2019001753-04.01.2023-#65741-JT-fim

  LOOP AT tl_0041 WHERE vbeln NE space.
    wl_ov_gerada = c_x.
    EXIT.
  ENDLOOP.

  IF sy-ucomm EQ 'MODIF' OR
    sy-ucomm EQ 'BTN_FUN'.
    CLEAR wg_header-vlrtot.
  ENDIF.

  SORT: tl_makt BY matnr.
  LOOP AT tl_0041.
    READ TABLE tl_makt
      WITH KEY matnr = tl_0041-matnr
               BINARY SEARCH.

    MOVE: tl_0041-posnr       TO tg_itens-posnr,
          tl_0041-matnr       TO tg_itens-matnr,
          tl_0041-zmeng       TO tg_itens-zmeng,
          tl_0041-zieme       TO tg_itens-zieme,
          tl_0041-dtvenc      TO tg_itens-dtvenc,
          tl_0041-zwert       TO tg_itens-zwert,
          tl_0041-calcu       TO tg_itens-calcu,
          tl_0041-trunit      TO tg_itens-trunit,
          tl_0041-siumb       TO tg_itens-siumb,
          tl_0041-trtot       TO tg_itens-trtot,
          tl_0041-compr       TO tg_itens-compr,
          tl_0041-vlrtot      TO tg_itens-vlrtot,
          tl_makt-maktx       TO tg_itens-maktx,
          tl_0041-auart       TO tg_itens-auart,
          tl_0041-spart       TO tg_itens-spart,
          tl_0041-werks       TO tg_itens-werks,
          tl_0041-charg       TO tg_itens-charg,
          tl_0041-inco1       TO tg_itens-inco1,
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
          tl_0041-cultura_apl TO tg_itens-cultura_apl,
          tl_0041-safra_apl   TO tg_itens-safra_apl,
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
          tl_0041-desconto    TO tg_itens-desconto,
          tl_0041-vlr_frete   TO tg_itens-vlr_frete,
          tl_0041-vl_unit     TO tg_itens-vl_unit,
          tl_0041-vlr_icms    TO tg_itens-vlr_icms,
          tl_0041-vlr_cofins  TO tg_itens-vlr_cofins,
          tl_0041-vlr_pis     TO tg_itens-vlr_pis,
          tl_0041-vlr_ajuste  TO tg_itens-vlr_ajuste,
          tl_0041-zwert_liqdo TO tg_itens-zwert_liqdo.

    IF sy-ucomm EQ 'MODIF' OR
      sy-ucomm EQ 'BTN_FUN'.
      SUBTRACT tg_itens-vlr_ajuste FROM tg_itens-vlrtot.
      CLEAR tg_itens-vlr_ajuste.
      ADD tg_itens-vlrtot TO wg_header-vlrtot.
    ENDIF.
*BREAK ABAP.


*    IF ( TL_0041-VL_UNIT IS INITIAL ).
*      MOVE TL_0041-ZWERT   TO TG_ITENS-VL_UNIT.
*    ELSE.
*      MOVE TL_0041-VL_UNIT TO TG_ITENS-VL_UNIT.
*    ENDIF.

    IF ( sy-ucomm EQ 'COPY' ).
**   Selecionar novamente os registros em caso de cópia para verificar se houve **
**   alteração nos valores                                                      **
      SELECT SINGLE vlr_venda
        FROM zsdt0036
        INTO (tg_itens-vl_unit)
       WHERE val_de       LE sy-datum
         AND val_ate      GE sy-datum
         AND matnr        EQ tg_itens-matnr
         AND waerk        EQ wg_header-waerk
         AND meins        EQ tg_itens-zieme
         AND inco1        EQ tg_itens-inco1
         AND safra        EQ wg_header-safra
         AND cultura      EQ wg_header-cultura
         AND loekz        EQ space
         AND werks_fornec EQ tg_itens-werks
         AND eliminado    EQ space
         AND bukrs        EQ wa_t001k-bukrs.

      IF ( tg_itens-inco1 = 'CIF').

**    Verifica se possuí frete **
        SELECT SINGLE matkl
          FROM mara
          INTO (wl_mara-matkl)
         WHERE matnr EQ tg_itens-matnr.

        SELECT SINGLE vlr_frete
          FROM zsdt0037
          INTO (wl_0037-vlr_frete)
         WHERE val_de          LE sy-datum
           AND val_ate         GE sy-datum
           AND meins           EQ tg_itens-zieme
           AND filial_origem   EQ tg_itens-werks
           AND filial_destino  EQ wg_header-vkbur
           AND matkl           EQ wl_mara-matkl
           AND waers           EQ 'BRL'
           AND bukrs           EQ wa_t001k-bukrs.

** COPIA
        PERFORM atualiza_frete CHANGING wl_0037-vlr_frete.

        ADD wl_0037-vlr_frete TO tg_itens-vl_unit.
      ENDIF.

      tg_itens-zwert         = tg_itens-vl_unit.
      tg_itens-vlrtot        = tg_itens-zmeng * tg_itens-zwert.
    ELSE.
      tg_itens-desc_absoluto = tl_0041-desc_absoluto.
    ENDIF.

    MOVE: tl_0041-mgcad    TO tg_itens-mgcad,
          tl_0041-mgefe    TO tg_itens-mgefe,
          tl_0041-vbeln    TO tg_itens-vbeln.
*          tl_0041-antec  to tg_itens-antec.



    IF wl_ov_gerada IS NOT INITIAL.
      REFRESH: style, tg_itens-style.

      wa_style-fieldname = 'VBELN'.
      IF tg_itens-vbeln IS NOT INITIAL.
        wa_style-style = alv_style_color_positive..
      ELSE.
        wa_style-style = alv_style_color_negative..
      ENDIF.
      INSERT  wa_style INTO TABLE style.
      CLEAR: wa_style.
      INSERT LINES OF style INTO TABLE tg_itens-style.
    ENDIF.
    APPEND tg_itens.
    CLEAR: tg_itens.
  ENDLOOP.

  LOOP AT tl_0186.

    MOVE-CORRESPONDING: tl_0186 TO tg_log.

    APPEND tg_log.
    CLEAR: tg_log, tl_0186.
  ENDLOOP.


  wg_header_old = wg_header.

ENDFORM.                    " MONTA_DADOS_DOC
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grava_dados .
  DATA: wl_input_0040 TYPE zsdt0040,
        tl_input_0041 TYPE TABLE OF zsdt0041 WITH HEADER LINE.

*Início - Sara Oikawa - 38859 - Agosto/2020
  DATA: wl_40_old TYPE zsdt0040,
        tl_41_old TYPE TABLE OF zsdt0041 WITH HEADER LINE.
*Fim - Sara Oikawa - 38859 - Agosto/2020

  DATA: vl_index TYPE sy-tabix.

  REFRESH: tl_input_0041.
  CLEAR: wl_input_0040, tl_input_0041.

  MOVE: wg_meio_pago      TO wg_header-meio_pago,
        convert_tratotsc  TO wg_header-trototsc.

  MOVE-CORRESPONDING wg_header TO wl_input_0040.

  MOVE: wg_area_ha               TO wl_input_0040-area_ha,
        space                    TO wl_input_0040-prec_cult,
        abap_false               TO wl_input_0040-job,
        sy-uname                 TO wl_input_0040-usnam,
        sy-datum                 TO wl_input_0040-data_atual,
        sy-uzeit                 TO wl_input_0040-hora_atual.

  CASE wg_save.
    WHEN c_dsc_abs.
      MOVE 'A' TO wl_input_0040-status.
    WHEN c_sacas.
      SELECT SINGLE status
        FROM zsdt0040
          INTO wl_input_0040-status
        WHERE doc_simulacao EQ wg_header-doc_simulacao.
  ENDCASE.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  SELECT SINGLE *
    FROM zsdt0040
      INTO wl_40_old
    WHERE doc_simulacao EQ wg_header-doc_simulacao.
  MOVE wl_40_old-status TO wl_input_0040-status.
  SELECT *
    FROM zsdt0041
    INTO TABLE tl_41_old
    WHERE doc_simulacao EQ wg_header-doc_simulacao.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  IF sy-ucomm EQ c_recalc.
    SELECT SINGLE status
      FROM zsdt0040
        INTO wl_input_0040-status
      WHERE doc_simulacao EQ wg_header-doc_simulacao.
  ENDIF.

  LOOP AT tg_itens.

    vl_index = sy-tabix.

    PERFORM busca_imposto CHANGING tg_itens.
    MODIFY tg_itens[] FROM tg_itens INDEX vl_index TRANSPORTING vlr_icms zwert_liqdo.

    MOVE: wg_header-doc_simulacao  TO tl_input_0041-doc_simulacao,
          tg_itens-posnr           TO tl_input_0041-posnr,
          tg_itens-matnr           TO tl_input_0041-matnr,
          tg_itens-zmeng           TO tl_input_0041-zmeng,
          tg_itens-zieme           TO tl_input_0041-zieme,
          tg_itens-zwert           TO tl_input_0041-zwert,
          tg_itens-calcu           TO tl_input_0041-calcu,
          tg_itens-trunit          TO tl_input_0041-trunit,
          tg_itens-siumb           TO tl_input_0041-siumb,
          tg_itens-trtot           TO tl_input_0041-trtot,
          tg_itens-compr           TO tl_input_0041-compr,
          tg_itens-vlrtot          TO tl_input_0041-vlrtot,
          tg_itens-auart           TO tl_input_0041-auart,
          tg_itens-spart           TO tl_input_0041-spart,
          tg_itens-werks           TO tl_input_0041-werks,
          tg_itens-charg           TO tl_input_0041-charg,
          tg_itens-inco1           TO tl_input_0041-inco1,
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
          tg_itens-cultura_apl     TO tl_input_0041-cultura_apl,
          tg_itens-safra_apl       TO tl_input_0041-safra_apl,
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
          tg_itens-desconto        TO tl_input_0041-desconto,
          tg_itens-vlr_ajuste      TO tl_input_0041-vlr_ajuste,
          tg_itens-desc_absoluto   TO tl_input_0041-desc_absoluto,
          tg_itens-vl_unit         TO tl_input_0041-vl_unit,
          tg_itens-mgcad           TO tl_input_0041-mgcad,
          tg_itens-mgefe           TO tl_input_0041-mgefe,
          tg_itens-dtvenc          TO tl_input_0041-dtvenc,
          tg_itens-vbeln           TO tl_input_0041-vbeln,
          tg_itens-vlr_frete       TO tl_input_0041-vlr_frete,
          tg_itens-vlr_icms        TO tl_input_0041-vlr_icms,
          tg_itens-zwert_liqdo     TO tl_input_0041-zwert_liqdo,
          tg_itens-j_1btxsdc       TO tl_input_0041-j_1btxsdc,
          tg_itens-j_1btaxlw1      TO tl_input_0041-j_1btaxlw1,
          tg_itens-j_1btaxlw5      TO tl_input_0041-j_1btaxlw5,
          tg_itens-j_1btaxlw4      TO tl_input_0041-j_1btaxlw4.
*          tg_itens-antec          to tl_input_0041-antec.

    APPEND tl_input_0041.
    CLEAR: tg_itens, tl_input_0041.
  ENDLOOP.

  DELETE FROM zsdt0040 WHERE doc_simulacao EQ wl_input_0040-doc_simulacao.
  DELETE FROM zsdt0041 WHERE doc_simulacao EQ wl_input_0040-doc_simulacao.

  " 06.02.2023 - Correção inserção data pagamento prev ->
  IF wl_input_0040-tpsim = 'VV'.
    wl_input_0040-dtprevpgto = wl_input_0040-dtvencov.
  ELSE.
    wl_input_0040-dtprevpgto = wl_input_0040-dtpgtcult.
  ENDIF.
  " 06.02.2023 - Correção inserção data pagamento prev -<

  MODIFY zsdt0040 FROM wl_input_0040.
  MODIFY zsdt0041 FROM TABLE tl_input_0041.

  COMMIT WORK.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  " Grava Vínculo Dados Compra Sigam
  IF wl_input_0040-tpsim(1) EQ c_tro(1).
    IF wg_0260-id_compra IS NOT INITIAL.
      MODIFY zsdt0260 FROM wg_0260.
    ELSE.
      DELETE FROM zsdt0260
       WHERE doc_simulacao EQ wg_0260-doc_simulacao.
    ENDIF.
  ENDIF.

* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

* Início - CS2019000925 - Sara Oikawa - Julho/2020
*> Ao realizar a alteração da quantidade de Sacas
*> Só fazer o disparo caso o simulador esteja aprovado.
*> O sistema deverá identificar se houve uma alteração no valor do campo "Valor Total".
*> Disparar para a tabela ZSDT0094, o valor de diferença.  Tanto para venda (VDI) quanto para Frete (FRI)
*> O valor da diferença deve levar em consideração a proporção entre os "Setor de Atividade" existentes no simulador.

  IF wg_save EQ c_sacas OR ( sy-ucomm EQ c_recalc AND wl_input_0040-tpsim EQ 'TS' )
                        OR ( sy-ucomm EQ c_recalc AND wl_input_0040-tpsim EQ 'TV' ).
    IF wl_input_0040-status EQ 'A'.

*     DISPARO DO HEDGE PARA VENDA
      IF wl_input_0040-waerk EQ 'BRL'.

        PERFORM dispara_hedge_ajuste_sacas TABLES tl_input_0041
                                    USING  wl_input_0040
                                           'VDI'.
      ENDIF.

*     DISPARO DO HEDGE PARA FRETE
      PERFORM dispara_hedge_ajuste_sacas TABLES tl_input_0041
                                  USING  wl_input_0040
                                         'FRI'.

    ENDIF.
  ENDIF.

* Fim  - CS2019000925 - Sara Oikawa - Julho/2020


  IF wg_save EQ c_sacas.
    PERFORM log_acao USING 'S' '' CHANGING lv_erro.

    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
    CHECK lv_erro IS INITIAL.
    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

  ENDIF.

  IF wg_save EQ c_dsc_abs.
    PERFORM atualiza_ordem_venda.
  ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
  IF wg_save EQ c_altjur.
    PERFORM log_acao USING 'J' '' CHANGING lv_erro.

    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
    CHECK lv_erro IS INITIAL.
    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<


* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
*    PERFORM f_log_edicao  TABLES tl_input_0041 tl_41_old
*                          USING  wl_input_0040 wl_40_old.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  ENDIF.

  IF wg_save EQ c_altadt.
    PERFORM log_acao USING 'D' '' CHANGING lv_erro.

    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
    CHECK lv_erro IS INITIAL.
    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<
* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
*    PERFORM f_log_edicao  TABLES tl_input_0041 tl_41_old
*                          USING  wl_input_0040 wl_40_old.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  ENDIF.
*Fim - Sara Oikawa - 38859 - Agosto/2020

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
  IF wg_save EQ c_altcsa.
    PERFORM log_acao USING 'C' '' CHANGING lv_erro.

    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
    CHECK lv_erro IS INITIAL.
    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
*    PERFORM f_log_edicao  TABLES tl_input_0041 tl_41_old
*                          USING  wl_input_0040 wl_40_old.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  IF wg_save EQ c_altcpg.
    PERFORM zf_altera_pagto_ov.
    DATA: lv_compl TYPE c LENGTH 20.
    CONCATENATE wl_40_old-tpsim 'para' wl_input_0040-tpsim INTO lv_compl SEPARATED BY space.
    PERFORM log_acao USING 'T' lv_compl CHANGING lv_erro.

    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
    CHECK lv_erro IS INITIAL.
    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

  ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8


* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  PERFORM f_log_edicao  TABLES tl_input_0041 tl_41_old
                        USING  wl_input_0040 wl_40_old.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  MESSAGE s836(sd) WITH 'Doc. Simulação'
                         wl_input_0040-doc_simulacao
                         ', criado/modificado com sucesso!'.

*  if sy-subrc is initial.
*    call function 'POPUP_TO_CONFIRM'
*      exporting
*        text_question  = text-i00
*      importing
*        answer         = wl_answer
*      exceptions
*        text_not_found = 1
*        others         = 2.
*
*    if not sy-subrc is initial.
*      message id sy-msgid type sy-msgty number sy-msgno
*              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    endif.
*
*    case wl_answer.
*      when '1'.
*        perform imprime_simulacao using wg_header-doc_simulacao.
*      when '2' or
*           'A'.
*
*    endcase.
*  endif.
  CALL FUNCTION 'DEQUEUE_EZSDT0040'
    EXPORTING
      doc_simulacao = wg_header-doc_simulacao.

  wg_acao = c_atual.
  CHECK sy-ucomm NE c_recalc.
  CHECK sy-ucomm NE 'BTN_FUN'.
  CHECK sy-ucomm NE 'BTN_FI'.


  PERFORM limpa_variavel USING c_atual.
  wg_acao = c_atual.
  s_acao = 'S_ATU'.
  LEAVE TO SCREEN 100.
ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_erros .

  TYPES: BEGIN OF ty_limit,
           setname  TYPE setnamenew,
           lineid   TYPE setline,
           descript TYPE setlintext,
           de	      TYPE setvalmin,
           ate      TYPE setvalmax,
         END OF ty_limit.

  DATA: wl_kna1     TYPE kna1,
        wl_matkl    TYPE mara-matkl,
        wl_t001w    TYPE t001w,
        wl_0038     TYPE zsdt0038,
        wl_linha(6),
        wl_data     TYPE zsdt0041-dtvenc,
        tl_0036     TYPE TABLE OF zsdt0036 WITH HEADER LINE,
        tl_0037     TYPE TABLE OF zsdt0037 WITH HEADER LINE,
        tl_tspa     TYPE TABLE OF tspa WITH HEADER LINE,
        it_limit    TYPE TABLE OF ty_limit,
        wl_tvko     TYPE tvko,
        wl_tvtw     TYPE tvtw,
        wl_0044     TYPE zsdt0044,
        tl_tvak     TYPE TABLE OF tvak WITH HEADER LINE,
        wl_tcurc    TYPE tcurc,
        tl_t001w    TYPE TABLE OF t001w WITH HEADER LINE,
        tl_marc     TYPE TABLE OF marc WITH HEADER LINE,
        b_kna1      TYPE kna1,
        b_knb1      TYPE knb1,
        erro        TYPE c.

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
  DATA: tl_0038     TYPE TABLE OF zsdt0038,
        wl_0038_apl TYPE zsdt0038,
        tl_0044     TYPE TABLE OF zsdt0044,
        wl_0044_apl TYPE zsdt0044.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

  REFRESH: tg_msg_ret, tl_0036, tl_0037, tl_tspa, tl_tvak, tl_t001w,
           tl_marc.
  CLEAR: tg_msg_ret, wl_kna1, wl_t001w, tl_0036, tl_0037, wl_linha,
         tl_tspa, wl_tvko, wl_tvtw, tl_tvak, wl_tcurc, wl_data.

*US 143453 - INICIO - PQ
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_ZSDT0044_09'
    TABLES
      set_values    = lt_vkorg
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  LOOP AT lt_vkorg INTO w_vkorg.
    r_vkorg-sign   = 'I'.
    r_vkorg-option = 'EQ'.
    r_vkorg-low    = w_vkorg-from.
    APPEND r_vkorg.
  ENDLOOP.

*US 143453 - INICIO - PQ


  PERFORM f_check_date_retroativa.

  SELECT SINGLE *
    FROM kna1
    INTO wl_kna1
     WHERE kunnr EQ wg_header-kunnr.

  SELECT SINGLE *
    FROM t001w
    INTO wl_t001w
     WHERE werks EQ wg_header-vkbur.

  SELECT SINGLE *
    FROM zsdt0038
    INTO wl_0038
     WHERE cultura EQ wg_header-cultura.

  SELECT SINGLE *
    FROM zsdt0044
    INTO wl_0044
     WHERE safra EQ wg_header-safra.

  SELECT SINGLE *
    FROM tvko
    INTO wl_tvko
     WHERE vkorg = wg_header-vkorg.

  SELECT SINGLE *
    FROM tvtw
    INTO wl_tvtw
     WHERE vtweg = wg_header-vtweg.

  SELECT SINGLE *
    FROM tcurc
    INTO wl_tcurc
     WHERE waers EQ wg_header-waerk.


  IF tg_itens[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0036
      INTO TABLE tl_0036
       FOR ALL ENTRIES IN tg_itens
       WHERE matnr        EQ tg_itens-matnr
*         and werks eq wg_header-vkbur
         AND val_de       LE sy-datum
         AND val_ate      GE sy-datum
         AND cultura      EQ wg_header-cultura
         AND waerk        EQ wg_header-waerk
         AND meins        EQ tg_itens-zieme
         AND safra        EQ wg_header-safra
         AND inco1        EQ tg_itens-inco1
         AND werks_fornec EQ tg_itens-werks
         AND loekz        EQ space
         AND eliminado    EQ space
         AND bukrs        EQ wa_t001k-bukrs.

    SELECT *
      FROM zsdt0036
      APPENDING TABLE tl_0036
       FOR ALL ENTRIES IN tg_itens
       WHERE matnr        EQ tg_itens-matnr
*         and werks eq wg_header-vkbur
         AND val_de       LE sy-datum
         AND val_ate      GE sy-datum
         AND cultura      EQ space
         AND waerk        EQ wg_header-waerk
         AND meins        EQ tg_itens-zieme
         AND safra        EQ wg_header-safra
         AND inco1        EQ tg_itens-inco1
         AND werks_fornec EQ tg_itens-werks
         AND loekz        EQ space
         AND eliminado    EQ space
         AND bukrs        EQ wa_t001k-bukrs.

    SELECT *
      FROM zsdt0037
      INTO TABLE tl_0037
       FOR ALL ENTRIES IN tg_itens
       WHERE val_de          LE sy-datum
         AND val_ate         GE sy-datum
         AND meins           EQ tg_itens-zieme
         AND filial_origem   EQ tg_itens-werks
         AND filial_destino  EQ wg_header-vkbur
         AND waers           EQ 'BRL'
         AND bukrs           EQ wa_t001k-bukrs.

    SELECT *
      FROM tvak
      INTO TABLE tl_tvak
      FOR ALL ENTRIES IN tg_itens
       WHERE auart = tg_itens-auart.

    SELECT *
      FROM tspa
      INTO TABLE tl_tspa
       FOR ALL ENTRIES IN tg_itens
       WHERE spart = tg_itens-spart.

    SELECT *
      FROM t001w
      INTO TABLE tl_t001w
       FOR ALL ENTRIES IN tg_itens
       WHERE werks EQ tg_itens-werks.

    SELECT *
    FROM marc
    INTO TABLE tl_marc
     FOR ALL ENTRIES IN tg_itens
     WHERE werks EQ tg_itens-werks
       AND matnr EQ tg_itens-matnr.

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
    SELECT *
      FROM zsdt0038
      INTO TABLE tl_0038
      FOR ALL ENTRIES IN tg_itens
      WHERE cultura EQ tg_itens-cultura_apl.

    SELECT *
      FROM zsdt0044
      INTO TABLE tl_0044
      FOR ALL ENTRIES IN tg_itens
      WHERE safra EQ tg_itens-safra_apl.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

  ENDIF.
*RSI - Case #CS0971604 Ajuste divisão frete
  IF erro_taxa_frete IS NOT INITIAL.
    DATA: lv_msg_erro_frete TYPE string.
    CONCATENATE 'Não Foi localizada a taxa de câmbio para conversão do frete para a empresa'
                erro_taxa_frete
                '. Procure a equipe do Insumos Corporativo para atualizar o cadastro.' INTO lv_msg_erro_frete SEPARATED BY space.
    MOVE: lv_msg_erro_frete       TO tg_msg_ret-msg,
          'WG_HEADER-TAXA_FRETE'       TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
    CLEAR: erro_taxa_frete.
  ENDIF.
*RSI - Case #CS0971604 Ajuste divisão frete
*Início - Sara Oikawa - 38859 - Agosto/2020
*  IF WG_HEADER-WAERK NE 'BRL' AND WG_HEADER-MEIO_PAGO = 'B'.
  IF wg_header-waerk NE 'BRL' AND wg_meio_pago = 'R'.
*Fim - Sara Oikawa - 38859 - Agosto/2020
    MOVE: TEXT-e49                TO tg_msg_ret-msg,
         'WG_HEADER-KUNNR'       TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.
** Emissor da fatura (KUNNR)
  IF wg_header-kunnr IS INITIAL.
    MOVE: TEXT-e01                TO tg_msg_ret-msg,
          'WG_HEADER-KUNNR'       TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wl_kna1-kunnr IS INITIAL.
    MOVE: TEXT-e02                TO tg_msg_ret-msg,
          'WG_HEADER-KUNNR'       TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.

  ELSEIF NOT wl_kna1-kunnr IS INITIAL.

    FREE erro.

*  Bloqueio Geral p/ todas empresas
    IF     NOT wl_kna1-sperr IS INITIAL.
      erro = abap_true.
*  Bloqueio de ordem centralizado para cliente
    ELSEIF NOT wl_kna1-aufsd IS INITIAL.
      erro = abap_true.
*  Bloqueio de remessa centralizado para cliente
    ELSEIF NOT wl_kna1-lifsd IS INITIAL.
      erro = abap_true.
*  Bloqueio centralizado de faturamento para cliente
    ELSEIF NOT wl_kna1-faksd IS INITIAL.
      erro = abap_true.
*  Bloqueio de contatos central para cliente
    ELSEIF NOT wl_kna1-cassd IS INITIAL.
      erro = abap_true.
*  Bloqueio de Pagamento
    ELSEIF NOT wl_kna1-sperz IS INITIAL.
      erro = abap_true.
*  Bloqueio central de eliminação para registro mestre
    ELSEIF NOT wl_kna1-nodel IS INITIAL.
      erro = abap_true.
    ENDIF.

    IF erro EQ abap_true.
      MOVE: TEXT-i02                TO tg_msg_ret-msg,
            'WG_HEADER-KUNNR'       TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

  ENDIF.

** Escritorio de vendas (VKBUR)
  IF wg_header-vkbur IS INITIAL.
    MOVE: TEXT-e03                TO tg_msg_ret-msg,
           'WG_HEADER-VKBUR'       TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wl_t001w-werks IS INITIAL.
    MOVE: TEXT-e04                TO tg_msg_ret-msg,
          'WG_HEADER-VKBUR'       TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

** Cultura (CULTURA)
  IF wg_header-cultura IS INITIAL.
    MOVE: TEXT-e05                TO tg_msg_ret-msg,
           'WG_HEADER-CULTURA'       TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wl_0038-cultura IS INITIAL.
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
*                                  MOVE: TEXT-E06                TO TG_MSG_RET-MSG,
    MOVE: TEXT-e59               TO tg_msg_ret-msg,
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
        'WG_HEADER-CULTURA'      TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
  ELSE.
    IF wl_0038-pagamento IS INITIAL.
      MOVE: TEXT-e60                TO tg_msg_ret-msg,
        'WG_HEADER-CULTURA'      TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    " Se 'Cultura Pagamento' do cabeçalho também está habilitada para ser uma 'Cultura Aplicação'
    CLEAR wg_cultura_apl.
    IF wl_0038-aplicacao IS NOT INITIAL.
      wg_cultura_apl = wg_header-cultura.
    ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

  ENDIF.

** Fazenda (FAZENDA)
  IF wg_header-fazenda IS INITIAL.
    MOVE: TEXT-e07                TO tg_msg_ret-msg,
          'WG_HEADER-FAZENDA'       TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.

  ENDIF.

** Vendedor (VENDEDOR)
  IF wg_header-vendedor IS INITIAL.
    MOVE: TEXT-e08                TO tg_msg_ret-msg,
          'WG_HEADER-VENDEDOR'       TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077
  ELSE.
    IF wg_desc_vendedor IS INITIAL.
      MOVE: TEXT-e72               TO tg_msg_ret-msg,
            'WG_HEADER-VENDEDOR'    TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077

  ENDIF.

** Area/há (AREA_HA)
  IF wg_area_ha IS INITIAL.
    MOVE: TEXT-e09                TO tg_msg_ret-msg,
          'WG_AREA_HA'            TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.

  ENDIF.

** Moeda (WAERK)
  IF wg_header-waerk IS INITIAL.
    MOVE: TEXT-e10                TO tg_msg_ret-msg,
          'WG_HEADER-WAERK'       TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.

  ELSEIF wl_tcurc-waers IS INITIAL.
    MOVE: TEXT-e25                TO tg_msg_ret-msg,
          'WG_HEADER-WAERK'       TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.

  ENDIF.

** Safra (SAFRA)
  IF wg_header-safra IS INITIAL.
    MOVE: TEXT-e11                TO tg_msg_ret-msg,
          'WG_HEADER-SAFRA'       TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wl_0044-safra IS INITIAL.
    MOVE: TEXT-e34                TO tg_msg_ret-msg,
          'WG_HEADER-SAFRA'       TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

** Tipo de Ordem (TPSIM)
  IF wg_header-tpsim IS INITIAL.
    MOVE: TEXT-e12                TO tg_msg_ret-msg,
          'WG_HEADER-TPSIM'       TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.

  ELSE.

    IF wg_header-tpsim(1) EQ c_tro(1).
*    or wg_header-tpsim(1) eq c_adt(1).
** Data de Entrega (DTENT)
      IF wg_header-dtent IS INITIAL.
        MOVE: TEXT-e37               TO tg_msg_ret-msg,
              'WG_HEADER-DTENT'      TO tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

** Frete da Cultura (FCULT)
      IF wg_header-fcult IS INITIAL.
        MOVE: TEXT-e38               TO tg_msg_ret-msg,
              'WG_HEADER-FCULT'      TO tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

** Tipo de Cultura (TPCULT)
      IF wg_header-tpcult IS INITIAL.
        MOVE: TEXT-e39                TO tg_msg_ret-msg,
              'WG_HEADER-TPCULT'      TO tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

    ENDIF.
  ENDIF.

  CASE wg_header-tpsim(2).
    WHEN 'VV'. "WSBB
*Início - Sara Oikawa - 38859 - Agosto/2020
*      IF WG_HEADER-MEIO_PAGO IS INITIAL.
      IF wg_meio_pago IS INITIAL.
*Fim - Sara Oikawa - 38859 - Agosto/2020
        tg_msg_ret-msg = |{ TEXT-e43 } 'Meio de Pagamento'| .
        MOVE 'WG_HEADER-MEIO_PAGO'      TO tg_msg_ret-field.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

      IF wg_header-dtvencov IS INITIAL.
        tg_msg_ret-msg = |{ TEXT-e43 } 'Data de Vencimento OV'| .
        MOVE 'WG_HEADER-DTVENCOV'      TO tg_msg_ret-field.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
      CLEAR wl_qtd_diasu.
      wl_dtlimite = sy-datum.

      WHILE wl_qtd_diasu < 3.   "(Até 3 dias uteis sem considerar a data da venda)

        wl_dtlimite = wl_dtlimite + 1.

        zcl_solicitacao_ov=>dia_util( EXPORTING p_vencimento = wl_dtlimite
                                        IMPORTING e_subrc  = DATA(lv_subrc)
                                     ).
        IF lv_subrc IS NOT INITIAL.
          wl_qtd_diasu = wl_qtd_diasu + 1.
        ENDIF.

      ENDWHILE.

      IF wg_header-dtvencov > wl_dtlimite.
        MOVE: 'WG_HEADER-DTVENCOV'   TO tg_msg_ret-field.
        tg_msg_ret-msg = TEXT-e52.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
*Fim - Sara Oikawa - 38859 - Agosto/2020

*** US - 81799 - Inicio - CBRAND
      IF wg_header-hbkid IS INITIAL AND wg_meio_pago = 'D'.
        tg_msg_ret-msg = |{ TEXT-e43 } 'Banco Empresa'| .
        MOVE 'WG_HEADER-HBKID'      TO tg_msg_ret-field.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
*** US - 81799 - Fim - CBRAND

    WHEN 'VP'. "WSBB
*Início - Sara Oikawa - 38859 - Agosto/2020
*      IF WG_HEADER-MEIO_PAGO IS INITIAL.
      IF wg_meio_pago IS INITIAL.
*Fim - Sara Oikawa - 38859 - Agosto/2020
        tg_msg_ret-msg = |{ TEXT-e43 } 'Meio de Pagamento'| .
        MOVE 'WG_HEADER-MEIO_PAGO'      TO tg_msg_ret-field.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

      IF wg_header-dtpgtcult IS INITIAL.
        tg_msg_ret-msg = |{ TEXT-e43 } 'Data de Vencimento'| .
        MOVE 'WG_HEADER-DTPGTCULT'      TO tg_msg_ret-field.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

*** US - 81799 - Inicio - CBRAND
      IF wg_header-hbkid IS INITIAL  AND wg_meio_pago = 'D'.
        tg_msg_ret-msg = |{ TEXT-e43 } 'Banco Empresa'| .
        MOVE 'WG_HEADER-HBKID'      TO tg_msg_ret-field.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
*** US - 81799 - Fim - CBRAND

    WHEN 'AD'.
      IF wg_header-dtvencov IS INITIAL.
        tg_msg_ret-msg = |{ TEXT-e43 } 'Data de Vencimento OV'| .
        MOVE 'WG_HEADER-DTVENCOV'      TO tg_msg_ret-field.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
*US 143453 - INICIO - PQ
      IF WG_HEADER-VKORG NOT IN r_vkorg.
        if  WG_HEADER-ERDAT >= '20240626'.  "US 144227 - PQ
            IF wg_header-dtvencov IS NOT INITIAL and wg_header-dt_entrega_fet IS NOT INITIAL.
               IF wg_header-dtvencov >= wg_header-dt_entrega_fet.
                  tg_msg_ret-msg = 'Data de Vencimento OV não pode ser igual ou superior a data de entrega do Fertilizante'.
                  MOVE 'WG_HEADER-DTVENCOV'      TO tg_msg_ret-field.
                  APPEND tg_msg_ret.
                  CLEAR: tg_msg_ret.
               ENDIF.
            ENDIF.
            IF wg_header-dtvencov IS NOT INITIAL and wg_header-dt_entrega_sem IS NOT INITIAL.
               IF wg_header-dtvencov >= wg_header-dt_entrega_sem.
                  tg_msg_ret-msg = 'Data de Vencimento OV não pode ser igual ou superior a data de entrega da Semente'.
                  MOVE 'WG_HEADER-DTVENCOV'      TO tg_msg_ret-field.
                  APPEND tg_msg_ret.
                  CLEAR: tg_msg_ret.
                ENDIF.
            ENDIF.
            IF wg_header-dtvencov IS NOT INITIAL and wg_header-dt_entrega_def IS NOT INITIAL.
               IF wg_header-dtvencov >= wg_header-dt_entrega_def.
                  tg_msg_ret-msg = 'Data de Vencimento OV não pode ser igual ou superior a data de entrega do Defensivo'.
                  MOVE 'WG_HEADER-DTVENCOV'      TO tg_msg_ret-field.
                  APPEND tg_msg_ret.
                  CLEAR: tg_msg_ret.
               ENDIF.
            ENDIF.
        endif.
      ENDIF.


    WHEN 'TS'.
      IF wg_header-antec LE 0.
        CONCATENATE TEXT-e43 'Tx. De Antecipação'         INTO tg_msg_ret-msg SEPARATED BY space.
        MOVE 'WG_HEADER-ANTEC'      TO tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

      IF NOT wg_header-dtpgtcult+6(2) BETWEEN 01 AND 25.
        APPEND VALUE #(
                        msg = 'Data Cultura fora do Limite estipulado "01/25"!'
                        field = 'WG_HEADER-DTPGTCULT'
                      ) TO tg_msg_ret.
      ENDIF.

      IF WG_HEADER-VKORG NOT IN r_vkorg.
        if  WG_HEADER-ERDAT >= '20240626'. "US 144227 - PQ
            IF wg_header-dtpgtcult IS NOT INITIAL and wg_header-dt_entrega_fet IS NOT INITIAL.
              IF wg_header-dtpgtcult >= wg_header-dt_entrega_fet.
                    tg_msg_ret-msg = 'A Dt Pagamento Milho/Soja não pode ser igual ou superior a data de entrega do Fertilizante'.
                    MOVE 'WG_HEADER-DTPGTCULT'      TO tg_msg_ret-field.
                    APPEND tg_msg_ret.
                    CLEAR: tg_msg_ret.
              ENDIF.
            ENDIF.
            IF wg_header-dtpgtcult IS NOT INITIAL and wg_header-dt_entrega_sem IS NOT INITIAL.
              IF wg_header-dtpgtcult >= wg_header-dt_entrega_sem.
                    tg_msg_ret-msg = 'A Dt Pagamento Milho/Soja não pode ser igual ou superior a data de entrega da Semente'.
                    MOVE 'WG_HEADER-DTPGTCULT'      TO tg_msg_ret-field.
                    APPEND tg_msg_ret.
                    CLEAR: tg_msg_ret.
              ENDIF.
            ENDIF.
            IF wg_header-dtpgtcult IS NOT INITIAL and wg_header-dt_entrega_def IS NOT INITIAL.
              IF wg_header-dtpgtcult >= wg_header-dt_entrega_def.
                    tg_msg_ret-msg = 'A Dt Pagamento Milho/Soja não pode ser igual ou superior a data de entrega do Defensivo'.
                    MOVE 'WG_HEADER-DTPGTCULT'      TO tg_msg_ret-field.
                    APPEND tg_msg_ret.
                    CLEAR: tg_msg_ret.
              ENDIF.
            ENDIF.
        endif.
      ENDIF.

      WHEN 'TV'.
        IF wg_header-vkorg IS not INITIAL.
          IF WG_HEADER-VKORG NOT IN r_vkorg.
             tg_msg_ret-msg = | A Condição de Pagamento 'Troca a Vista' não pode ser utilizada em Vendas da Organização { WG_HEADER-VKORG } |.
             MOVE 'WG_HEADER-TPSIM'      TO tg_msg_ret-field.
             APPEND tg_msg_ret.
             CLEAR: tg_msg_ret.
          ENDIF.
        ENDIF.
*US 143453 - FIM - PQ
  ENDCASE.

* COMENTADO AQUI PQ A COLOQUEI VALIDADAÇÃO PARA DENTRO DO CASE ACIMA - US 143453 - INICIO - PQ
*  IF wg_header-tpsim(2) EQ 'TS'.
** Tx. De Antecipação
*    IF wg_header-antec LE 0.
*      CONCATENATE TEXT-e43 'Tx. De Antecipação'         INTO tg_msg_ret-msg SEPARATED BY space.
*      MOVE 'WG_HEADER-ANTEC'      TO tg_msg_ret-field.
*
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.
*
*    ENDIF.
*
*    IF NOT wg_header-dtpgtcult+6(2) BETWEEN 01 AND 25.
*      APPEND VALUE #(
*                      msg = 'Data Cultura fora do Limite estipulado "01/25"!'
*                      field = 'WG_HEADER-DTPGTCULT'
*                    ) TO tg_msg_ret.
*    ENDIF.
*  ENDIF.
* COMENTADO AQUI PQ A COLOQUEI VALIDADAÇÃO PARA DENTRO DO CASE ACIMA - US 143453 - FIM - PQ

  IF NOT wg_header-dtent IS INITIAL.
*    ZCL_SOLICITACAO_OV=>DIA_UTIL( EXPORTING P_VENCIMENTO = WG_HEADER-DTENT
*                                    IMPORTING E_SUBRC    = DATA(S_SUBRC)
*                                ).
*    IF S_SUBRC IS INITIAL.
*      MOVE: 'WG_HEADER-DTENT'   TO TG_MSG_RET-FIELD.
*      TG_MSG_RET-MSG = | Data de Entrega não é dia Útil!|.
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ENDIF.

*    IF wg_header-dtent < sy-datum.
*      MOVE: 'WG_HEADER-DTENT'   TO tg_msg_ret-field.
*      tg_msg_ret-msg = | Data de Entrega não pode ser retroativa!|.
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.
*    ENDIF.

  ENDIF.

  DATA(data_limite) = sy-datum + 730.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  IF wg_save NE c_altcpg.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

    IF NOT wg_header-dt_entrega_fet IS INITIAL.
      zcl_solicitacao_ov=>dia_util( EXPORTING p_vencimento = wg_header-dt_entrega_fet
                                      IMPORTING e_subrc    = DATA(s_subrc)
                                  ).
      IF s_subrc IS INITIAL.
        MOVE: 'WG_HEADER-DT_ENTREGA_FET'   TO tg_msg_ret-field.
        tg_msg_ret-msg = | Data de Entrega de Fertilizante não é dia Útil!|.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

*      IF wg_header-dt_entrega_fet < sy-datum.
*        MOVE: 'WG_HEADER-DT_ENTREGA_FET'   TO tg_msg_ret-field.
*        tg_msg_ret-msg = | Data de Entrega de Fertilizante não pode ser retroativa!|.
*        APPEND tg_msg_ret.
*        CLEAR: tg_msg_ret.
*      ENDIF.

      IF wg_header-dt_entrega_fet > data_limite.
        MOVE: 'WG_HEADER-DT_ENTREGA_FET'   TO tg_msg_ret-field.
        tg_msg_ret-msg = | Data de Entrega de Fertilizante não pode ser superior a DOIS anos!|.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

    ENDIF.

    IF NOT wg_header-dt_entrega_sem IS INITIAL.
      zcl_solicitacao_ov=>dia_util( EXPORTING p_vencimento = wg_header-dt_entrega_sem
                                      IMPORTING e_subrc    = s_subrc
                                  ).
      IF s_subrc IS INITIAL.
        MOVE: 'WG_HEADER-DT_ENTREGA_SEM'   TO tg_msg_ret-field.
        tg_msg_ret-msg = | Data de Entrega da Semente não é dia Útil!|.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

*      IF wg_header-dt_entrega_sem < sy-datum.
*        MOVE: 'WG_HEADER-DT_ENTREGA_SEM'   TO tg_msg_ret-field.
*        tg_msg_ret-msg = | Data de Entrega de Semente não pode ser retroativa!|.
*        APPEND tg_msg_ret.
*        CLEAR: tg_msg_ret.
*      ENDIF.

      IF wg_header-dt_entrega_sem > data_limite.
        MOVE: 'WG_HEADER-DT_ENTREGA_SEM'   TO tg_msg_ret-field.
        tg_msg_ret-msg = | Data de Entrega de Semente não pode ser superior a DOIS anos!|.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

    ENDIF.


    IF NOT wg_header-dt_entrega_def IS INITIAL.
      zcl_solicitacao_ov=>dia_util( EXPORTING p_vencimento = wg_header-dt_entrega_def
                                      IMPORTING e_subrc    = s_subrc
                                  ).
      IF s_subrc IS INITIAL.
        MOVE: 'WG_HEADER-DT_ENTREGA_DEF'   TO tg_msg_ret-field.
        tg_msg_ret-msg = | Data de Entrega do Defensivos não é dia Útil!|.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

*      IF wg_header-dt_entrega_def < sy-datum.
*        MOVE: 'WG_HEADER-DT_ENTREGA_DEF'   TO tg_msg_ret-field.
*        tg_msg_ret-msg = | Data de Entrega de Defensivos não pode ser retroativa!|.
*        APPEND tg_msg_ret.
*        CLEAR: tg_msg_ret.
*      ENDIF.

      IF wg_header-dt_entrega_def > data_limite.
        MOVE: 'WG_HEADER-DT_ENTREGA_DEF'   TO tg_msg_ret-field.
        tg_msg_ret-msg = | Data de Entrega de Defensivos não pode ser superior a DOIS anos!|.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

    ENDIF.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

  IF NOT wg_header-dtpgtcult IS INITIAL.
    zcl_solicitacao_ov=>dia_util( EXPORTING p_vencimento = wg_header-dtpgtcult
                                    IMPORTING e_subrc    = s_subrc
                                ).
    IF s_subrc IS INITIAL.
      MOVE: 'WG_HEADER-DTPGTCULT'   TO tg_msg_ret-field.
      tg_msg_ret-msg = | Data Informada Vencimento SJ/ML não é Dia Útil!|.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
*
*    IF wg_header-dtpgtcult < sy-datum.
*      MOVE: 'WG_HEADER-DTPGTCULT'   TO tg_msg_ret-field.
*      tg_msg_ret-msg = | Data Informada Vencimento SJ/ML não pode ser retroativa!|.
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.
*    ENDIF.

  ENDIF.

  IF NOT wg_header-dtvencov IS INITIAL.
    zcl_solicitacao_ov=>dia_util( EXPORTING p_vencimento = wg_header-dtvencov
                                      IMPORTING e_subrc  = s_subrc
                                ).
    IF s_subrc IS INITIAL.
      MOVE: 'WG_HEADER-DTVENCOV'   TO tg_msg_ret-field.
      tg_msg_ret-msg = | Data Informada no Venc OV não é Dia Útil!|.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

*    IF wg_header-dtvencov < sy-datum.
*      MOVE: 'WG_HEADER-DTVENCOV'   TO tg_msg_ret-field.
*      tg_msg_ret-msg = | Data Informada no Venc OV não pode ser retroativa!|.
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.
*    ENDIF.

    IF wg_header-dtvencov+6(2) > 25  .
      IF ( wg_header-tpsim(2) NE 'VV' ) AND ( wg_header-tpsim(2) NE 'VP' ).
        MOVE: 'WG_HEADER-DTVENCOV'   TO tg_msg_ret-field.
        tg_msg_ret-msg = | Data do Venc OV não pode ser superior ao dia 25 de qualquer mês!|.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

  ENDIF.

  IF NOT wg_header-dtinijuros IS INITIAL.
    zcl_solicitacao_ov=>dia_util( EXPORTING p_vencimento = wg_header-dtinijuros
                                      IMPORTING e_subrc  = s_subrc
                                ).
    IF s_subrc IS INITIAL.
      MOVE: 'WG_HEADER-DTINIJUROS'   TO tg_msg_ret-field.
      tg_msg_ret-msg = | Data Inicial de Juros Informada não é Dia Útil!|.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

** Dt.Pagamento da Cultura (DTPGTCULT)
  IF wg_header-dtpgtcult IS INITIAL AND wg_header-tpsim(2) NE 'VF'
                                    AND wg_header-tpsim(2) NE 'BN'
                                    AND wg_header-tpsim(2) NE 'VV'.
    MOVE: "TEXT-E39                TO TG_MSG_RET-MSG,
          'WG_HEADER-DTPGTCULT'   TO tg_msg_ret-field.
    CONCATENATE TEXT-e43 ' " ' wg_desc_dat' "' INTO tg_msg_ret-msg.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.

  ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
  IF wg_header-dtinijuros IS NOT INITIAL AND wg_header-dtpgtcult IS NOT INITIAL
     AND  wg_header-tpsim(2) EQ 'VP'.
    IF wg_header-dtinijuros < wg_header-dtpgtcult.
      MOVE: 'WG_HEADER-DTINIJUROS'   TO tg_msg_ret-field.
      tg_msg_ret-msg = TEXT-e51.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.
*Fim - Sara Oikawa - 38859 - Agosto/2020

  IF wg_header-kursf IS INITIAL AND wg_header-tpsim(2) EQ 'VF'.
    MOVE: "TEXT-E43                TO TG_MSG_RET-MSG,
          'WG_HEADER-KURSF'   TO tg_msg_ret-field.
    CONCATENATE TEXT-e43 ' Taxa de Câmbio' INTO tg_msg_ret-msg.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  IF wg_header-tpsim EQ 'VP'.
    IF  wg_header-dtinijuros IS INITIAL.
      MOVE: "TEXT-E39                TO TG_MSG_RET-MSG,
         'WG_HEADER-DTINIJUROS'   TO tg_msg_ret-field.
      CONCATENATE TEXT-e43 ' "Data Inicio Juros"' INTO tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

** Organização vendas (VKORG)
  IF wg_header-vkorg IS INITIAL.
    MOVE: TEXT-e18                TO tg_msg_ret-msg,
          'WG_HEADER-VKORG'       TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wl_tvko-vkorg IS INITIAL.
    MOVE: TEXT-e19                TO tg_msg_ret-msg,
          'WG_HEADER-VKORG'       TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

** Canal de distribuição (VTWEG)
  IF wg_header-vtweg IS INITIAL.
    MOVE: TEXT-e20                TO tg_msg_ret-msg,
          'WG_HEADER-VTWEG'       TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wl_tvtw-vtweg IS INITIAL.
    MOVE: TEXT-e21                TO tg_msg_ret-msg,
          'WG_HEADER-VTWEG'       TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.



  IF wg_header-tpsim(1) EQ c_tro(1).
** Preço Sc. (Bruto) (PREC_CULT)
    IF wg_header-prec_ant_cult IS INITIAL.
      MOVE: TEXT-e26                  TO tg_msg_ret-msg,
            'WG_HEADER-PREC_ANT_CULT' TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

    IF wg_0260-id_compra IS NOT INITIAL.
      PERFORM zf_valida_compra_sigam USING 'X'
                                     CHANGING tg_msg_ret-msg.
      IF tg_msg_ret-msg IS NOT INITIAL.
        MOVE  'WG_0260-ID_COMPRA'   TO tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.
    ENDIF.

* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6


  ELSEIF wg_header-tpsim(1) EQ c_adt(1).
** Juros ao Ano(%) (JUROS_ANO)
    IF wg_header-juros_ano IS INITIAL.
      MOVE: TEXT-e27                TO tg_msg_ret-msg,
            'WG_HEADER-JUROS_ANO'   TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
** Vlr.Adiantado por Sc. (VLR_ADTO)
    IF wg_header-vlr_adto IS INITIAL.
      MOVE: TEXT-e28                TO tg_msg_ret-msg,
            'WG_HEADER-VLR_ADTO'   TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
** Adiantamento P/Hectare (ADTO_HA)
    IF wg_header-adto_ha IS INITIAL.
      MOVE: TEXT-e29                TO tg_msg_ret-msg,
            'WG_HEADER-ADTO_HA'   TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

  ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
  IF wg_header-tpsim(1) EQ  'V' OR
     wg_header-tpsim    EQ  'PM'.
** Juros ao Ano(%) (JUROS_ANO)
    IF wg_header-juros_ano IS INITIAL.
      MOVE: TEXT-e27                TO tg_msg_ret-msg,
            'WG_HEADER-JUROS_ANO'   TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.
*Fim - Sara Oikawa - 38859 - Agosto/2020


  READ TABLE tg_itens WITH KEY spart = '02'.
  IF sy-subrc IS INITIAL.
    IF wg_header-dt_entrega_fet IS INITIAL.
      tg_msg_ret-msg = |{ TEXT-i01 } da Data de Fertilizante |.
      tg_msg_ret-field = 'WG_HEADER-DT_ENTREGA_FET'.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  READ TABLE tg_itens WITH KEY spart = '03'.
  IF sy-subrc IS INITIAL.
    IF wg_header-dt_entrega_def IS INITIAL.
      tg_msg_ret-msg = |{ TEXT-i01 } da Data de Defensivos |.
      tg_msg_ret-field = 'WG_HEADER-DT_ENTREGA_DEF'.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  READ TABLE tg_itens WITH KEY spart = '04'.
  IF sy-subrc IS INITIAL.
    IF wg_header-dt_entrega_sem IS INITIAL.
      tg_msg_ret-msg = |{ TEXT-i01 } da Data de Sementes |.
      tg_msg_ret-field = 'WG_HEADER-DT_ENTREGA_SEM'.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.


*  if wg_header-venci is initial.
*    move: 'VENCI'  to tg_msg_ret-field,
*          'GRID1'  to tg_msg_ret-obj,
*          wl_linha to tg_msg_ret-tabix.
*
*    concatenate text-e16 ' LINHA: ' wl_linha into  tg_msg_ret-msg.
*    append tg_msg_ret.
*    clear: tg_msg_ret.
*  endif.

  IF tg_itens[] IS INITIAL.
    MOVE: TEXT-e13                TO tg_msg_ret-msg.
*      'WG_HEADER-TPSIM'       to tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  SORT: tl_0036  BY matnr waerk meins safra inco1 werks_fornec,
        tl_tvak  BY auart,
        tl_tspa  BY spart,
        tl_t001w BY werks,
        tl_marc  BY werks matnr.

  total_sac = 0.
  sum_vlrtot = 0.

  LOOP AT tg_itens.
    wl_linha = sy-tabix.

    READ TABLE tl_0036
      WITH KEY matnr = tg_itens-matnr
               waerk = wg_header-waerk
               meins = tg_itens-zieme
               safra = wg_header-safra
               inco1 = tg_itens-inco1
               werks_fornec = tg_itens-werks
               BINARY SEARCH.

    IF sy-subrc IS NOT INITIAL.
      MOVE: 'MATNR'  TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE TEXT-e14 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.

    ELSE.
      IF tg_itens-zmeng IS INITIAL.
        MOVE: 'ZMENG'  TO tg_msg_ret-field,
              'GRID1'  TO tg_msg_ret-obj,
              wl_linha TO tg_msg_ret-tabix.

        CONCATENATE TEXT-e15 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

** Tipo de documento (AUART)
    CLEAR: tl_tvak.
    READ TABLE tl_tvak
      WITH KEY auart = tg_itens-auart
               BINARY SEARCH.

    IF tg_itens-auart IS INITIAL.
      MOVE: 'AUART'  TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE TEXT-e17 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.

    ELSEIF tl_tvak-auart IS INITIAL.
      MOVE: 'AUART'  TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE TEXT-e24 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.

    ENDIF.

** Setor de atividade (SPART)
    CLEAR: tl_tspa.
    READ TABLE tl_tspa
     WITH KEY spart = tg_itens-spart
              BINARY SEARCH.

    IF tg_itens-spart IS INITIAL.
      MOVE: 'SPART'  TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE TEXT-e22 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSEIF tl_tspa-spart IS INITIAL.
      MOVE: 'SPART'  TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE TEXT-e23 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
* Quantidade Embalagem (para Defensivos)

    IF tg_itens-spart EQ '03'.
      IF NOT tg_itens-matnr IS INITIAL.

        PERFORM zf_busca_qtd_embalagem_zdef.

        IF wg_groes_string EQ c_x.
          MOVE: 'ZMENG'      TO tg_msg_ret-field,
                'GRID1'      TO tg_msg_ret-obj,
                 wl_linha    TO tg_msg_ret-tabix.
          " 'A quantidade da embalagem do material XXXXX informada no cadastro mestre, deve ser numérica.'
          CONCATENATE TEXT-e61 wg_matnr TEXT-e62 ' LINHA: ' wl_linha
                 INTO  tg_msg_ret-msg SEPARATED BY space.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.

        ELSE.
          IF  wg_groes_dec IS INITIAL.
            MOVE: 'ZMENG'      TO tg_msg_ret-field,
                  'GRID1'      TO tg_msg_ret-obj,
                   wl_linha    TO tg_msg_ret-tabix.
            " 'O Cadastro do Material  XXXXXX  está sem a quantidade da embalagem no cadastro mestre.'
            CONCATENATE TEXT-e63 wg_matnr TEXT-e64 ' LINHA: ' wl_linha
                   INTO  tg_msg_ret-msg SEPARATED BY space.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.

          ELSE.
            wg_qtd_prop = frac( tg_itens-zmeng / wg_groes_dec ).
            IF wg_qtd_prop IS NOT INITIAL.
              MOVE: 'ZMENG'   TO tg_msg_ret-field,
                    'GRID1'   TO tg_msg_ret-obj,
                    wl_linha  TO tg_msg_ret-tabix.
              " 'A quantidade informada para o item, não é proporcional à quantidade de XXXXX XX da embalagem do material.'
              CONCATENATE TEXT-e65 wg_groes wg_meins TEXT-e66 ' LINHA: ' wl_linha
                     INTO  tg_msg_ret-msg SEPARATED BY space.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

* Validar Cultura e Safra Aplicação

    IF tg_itens-cultura_apl IS NOT INITIAL.

      CLEAR wl_0038_apl.
      READ TABLE tl_0038 INTO wl_0038_apl WITH KEY cultura = tg_itens-cultura_apl.

      IF sy-subrc IS NOT INITIAL.
        MOVE: 'CULTURA_APL'  TO tg_msg_ret-field,
              'GRID1'  TO tg_msg_ret-obj,
               wl_linha TO tg_msg_ret-tabix.
        "Cultura de Aplicação informada não existe.
        CONCATENATE TEXT-e67 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSEIF wl_0038_apl-aplicacao IS INITIAL.
        MOVE: 'CULTURA_APL'  TO tg_msg_ret-field,
              'GRID1'  TO tg_msg_ret-obj,
               wl_linha TO tg_msg_ret-tabix.
        "Cultura de Aplicação informada não habilitada.
        CONCATENATE TEXT-e68 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF tg_itens-safra_apl IS NOT INITIAL.

      CLEAR wl_0044_apl.
      READ TABLE tl_0044 INTO wl_0044_apl WITH KEY safra = tg_itens-safra_apl.

      IF sy-subrc IS NOT INITIAL.
        MOVE: 'SAFRA_APL'  TO tg_msg_ret-field,
              'GRID1'  TO tg_msg_ret-obj,
               wl_linha TO tg_msg_ret-tabix.
        "Safra de Aplicação não é válida.
        CONCATENATE TEXT-e69 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4


** Data Vencimento (DTVENC)
    wl_data = tl_0036-dtvenc.
    ADD 92 TO wl_data.
    IF tg_itens-dtvenc IS INITIAL.
      MOVE: 'DTVENC'  TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE TEXT-e40 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.

*      ZCL_SOLICITACAO_OV=>DIA_UTIL( EXPORTING P_VENCIMENTO = TG_ITENS-DTVENC
*                                            IMPORTING E_SUBRC  = S_SUBRC
*                                      ).
*
*      IF S_SUBRC IS INITIAL.
*        MOVE: 'DTVENC'  TO TG_MSG_RET-FIELD,
*              'GRID1'  TO TG_MSG_RET-OBJ,
*              WL_LINHA TO TG_MSG_RET-TABIX.
*        TG_MSG_RET-MSG = |Data de Vencimento do Iten não é dia Útil! LINHA { WL_LINHA }.|.
*        APPEND TG_MSG_RET.
*        CLEAR: TG_MSG_RET.
*      ENDIF.

      IF tg_itens-dtvenc GT wl_data.
        MOVE: 'DTVENC'  TO tg_msg_ret-field,
              'GRID1'  TO tg_msg_ret-obj,
              wl_linha TO tg_msg_ret-tabix.

        CONCATENATE TEXT-e41 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

    ENDIF.

******* Centro fornecedor (WERKS)
*****    clear: tl_t001w.
*****    read table tl_t001w
*****     with key werks = tg_itens-werks
*****              binary search.
*****
*****    if tg_itens-werks is initial.
*****      move: 'WERKS'  to tg_msg_ret-field,
*****            'GRID1'  to tg_msg_ret-obj,
*****            wl_linha to tg_msg_ret-tabix.
*****
*****      concatenate text-e30 ' LINHA: ' wl_linha into  tg_msg_ret-msg.
*****      append tg_msg_ret.
*****      clear: tg_msg_ret.
*****    elseif tl_t001w-werks is initial.
*****      move: 'WERKS'  to tg_msg_ret-field,
*****            'GRID1'  to tg_msg_ret-obj,
*****            wl_linha to tg_msg_ret-tabix.
*****
*****      concatenate text-e31 ' LINHA: ' wl_linha into  tg_msg_ret-msg.
*****      append tg_msg_ret.
*****      clear: tg_msg_ret.
*****    endif.

*** Verifica se o material esta expandido para o centro
*    clear: tl_marc.
*    read table tl_marc
*     with key werks = tg_itens-werks
*              matnr = tg_itens-matnr
*              binary search.
*
*    if sy-subrc is not initial.
*      move: 'MATNR'  to tg_msg_ret-field,
*            'GRID1'  to tg_msg_ret-obj,
*            wl_linha to tg_msg_ret-tabix.
*
*      concatenate text-e32 ' LINHA: ' wl_linha into  tg_msg_ret-msg.
*      append tg_msg_ret.
*      clear: tg_msg_ret.
*    endif.

    SELECT SINGLE matkl
    FROM mara
    INTO wl_matkl
    WHERE matnr EQ tg_itens-matnr.

    IF tg_itens-inco1 IS INITIAL.
      MOVE: 'INCO1' TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE TEXT-e33 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.

    ELSEIF tg_itens-inco1 EQ 'CIF'.

      READ TABLE tl_0037
            WITH KEY filial_origem  = tg_itens-werks
                     meins          = tg_itens-zieme
                     filial_destino = wg_header-vkbur
                     waers          = 'BRL'
                     matkl          = wl_matkl.
      IF sy-subrc IS NOT INITIAL.
        MOVE: 'INCO1' TO tg_msg_ret-field,
              'GRID1'  TO tg_msg_ret-obj,
              wl_linha TO tg_msg_ret-tabix.

        CONCATENATE TEXT-e35 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*    ELSEIF TG_ITENS-INCO1 EQ 'CPT'.
*
*      IF WL_MATKL EQ '658445'.
*        READ TABLE TL_0037
*         WITH KEY FILIAL_ORIGEM  = TG_ITENS-WERKS
*           MEINS          = TG_ITENS-ZIEME
*           FILIAL_DESTINO = WG_HEADER-VKBUR
*           WAERS          = 'BRL'
*           MATKL          = WL_MATKL.
*
*        IF SY-SUBRC IS NOT INITIAL.
*          MOVE: 'INCO1' TO TG_MSG_RET-FIELD,
*                'GRID1'  TO TG_MSG_RET-OBJ,
*                WL_LINHA TO TG_MSG_RET-TABIX.
*
*          CONCATENATE TEXT-E35 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*          APPEND TG_MSG_RET.
*          CLEAR: TG_MSG_RET.
*
*        ENDIF.
*      ENDIF.
    ENDIF.

    IF tg_itens-zwert IS INITIAL.
      MOVE: 'ZWERT'  TO tg_msg_ret-field,
      'GRID1'  TO tg_msg_ret-obj,
      wl_linha TO tg_msg_ret-tabix.

      CONCATENATE 'É obrigatório o preenchimento do campo "Vlr negociado".' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

* Valida os campos AUART da grid1 verificando se é igual ao TPSIM ZFUT e ZBON
    CASE wg_header-tpsim(2).
      WHEN 'VF'.
        IF tg_itens-auart NE 'ZFUT'.
          MOVE: 'AUART'  TO tg_msg_ret-field,
                'GRID1'  TO tg_msg_ret-obj,
                wl_linha TO tg_msg_ret-tabix.

          CONCATENATE 'Tipo da Ordem de Venda diferente de "ZFUT".' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.

      WHEN 'BN'.
        IF tg_itens-auart NE 'ZBON'.
          MOVE: 'AUART'  TO tg_msg_ret-field,
                'GRID1'  TO tg_msg_ret-obj,
                wl_linha TO tg_msg_ret-tabix.

          CONCATENATE 'Tipo da Ordem de Venda diferente de "ZBON".' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
*Início - Sara Oikawa - 38859 - Agosto/2020
      WHEN OTHERS.
        IF tg_itens-auart EQ 'ZFUT'.
          MOVE: 'AUART'  TO tg_msg_ret-field,
                'GRID1'  TO tg_msg_ret-obj,
                wl_linha TO tg_msg_ret-tabix.

          CONCATENATE 'Tipo da Ordem de Venda deve ser diferente de "ZFUT".' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
*Fim - Sara Oikawa - 38859 - Agosto/2020
    ENDCASE.


    IF tg_itens-vlr_frete IS INITIAL.
      IF 'CIF' EQ tg_itens-inco1.


        MOVE: 'VLR_FRETE'  TO tg_msg_ret-field,
                  'GRID1'  TO tg_msg_ret-obj,
                  wl_linha TO tg_msg_ret-tabix.

        CONCATENATE 'Frete não agregado ao Valor do produto.  LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

*      IF 'CPT' EQ TG_ITENS-INCO1 AND TG_ITENS-SPART EQ '03'.
*
*        MOVE: 'VLR_FRETE'  TO TG_MSG_RET-FIELD,
*              'GRID1'  TO TG_MSG_RET-OBJ,
*              WL_LINHA TO TG_MSG_RET-TABIX.
*
*        TG_MSG_RET-MSG = | 'Não Localizado o Valor do Frete. Necessário para o lançamento do HEDGE.  LINHA: { WL_LINHA } |.
*        APPEND TG_MSG_RET.
*        CLEAR: TG_MSG_RET.
*      ENDIF.
    ENDIF.

*    Estabelece um Teto Maximo para variação de correção do Troca Total/sac
    ADD tg_itens-trtot TO total_sac.
    ADD tg_itens-vlrtot TO sum_vlrtot.

  ENDLOOP.

  LOOP AT tg_trans ASSIGNING FIELD-SYMBOL(<w90>)
  WHERE categoria EQ 'O'
  AND estorno EQ abap_false
  AND flag EQ abap_true.
    ADD <w90>-desc_absoluto TO sum_vlrtot.
  ENDLOOP.

  IF NOT line_exists( tg_itens[ spart = '04' ] ).
    FREE wg_header-dt_entrega_sem.
  ENDIF.

  IF NOT line_exists( tg_itens[ spart = '03' ] ).
    FREE wg_header-dt_entrega_def.
  ENDIF.

  IF NOT line_exists( tg_itens[ spart = '02' ] ).
    FREE wg_header-dt_entrega_fet.
  ENDIF.

  "Verifica se o Valor total dos Itens + o desconto absoluto que foram agregado é DIFERENTE do valor total do Simulador.
  IF sum_vlrtot NE wg_header-vlrtot.
    MOVE: TEXT-e45            TO tg_msg_ret-msg,
          'WG_HEADER-VLRTOT'  TO tg_msg_ret-field.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

*WG_HEADER-CULTURA
* Set para limitar a quantidade do Total Saca ML SJ AL
  IF NOT convert_tratotsc IS INITIAL AND wg_header-cultura IS NOT INITIAL.
    total_sac = total_sac - convert_tratotsc.

    SELECT a~setname a~lineid a~descript b~valto b~valfrom
      FROM setlinet AS a
      INNER JOIN setleaf AS b ON b~setname EQ a~setname AND
                                 b~lineid  EQ a~lineid
      INTO TABLE it_limit
       WHERE a~setname EQ 'MAGGI_ZSDT0044_07'
         AND a~langu   EQ sy-langu
         AND a~descript EQ wg_header-cultura.

    SORT it_limit BY descript.
    DELETE ADJACENT DUPLICATES FROM it_limit COMPARING descript.

    IF it_limit IS NOT INITIAL.
      IF total_sac NOT BETWEEN it_limit[ 1 ]-de AND it_limit[ 1 ]-ate.

        MOVE: TEXT-e44            TO tg_msg_ret-msg,
              'CONVERT_TRATOTSC'  TO tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.
    ELSE.
      MESSAGE 'Limite não Cadastrado para a Cultura! Set "MAGGI_ZSDT0044_07"' TYPE 'S'.
    ENDIF.
  ENDIF.

*  Inicio Bloqueio cliente com restrições CS2016000357 WB
  IF NOT wg_header-kunnr IS INITIAL.

    SELECT SINGLE *
      FROM kna1
        INTO b_kna1
          WHERE kunnr EQ wg_header-kunnr.

*  Bloqueio Geral p/ todas empresas
    IF     NOT b_kna1-sperr IS INITIAL.
      MOVE: TEXT-001            TO tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.

*  Bloqueio de ordem centralizado para cliente
    ELSEIF NOT b_kna1-aufsd IS INITIAL.
      MOVE: TEXT-001 TO tg_msg_ret-msg. APPEND tg_msg_ret. CLEAR: tg_msg_ret.
*  Bloqueio de remessa centralizado para cliente
    ELSEIF NOT b_kna1-lifsd IS INITIAL.
      MOVE: TEXT-001 TO tg_msg_ret-msg. APPEND tg_msg_ret. CLEAR: tg_msg_ret.
*  Bloqueio centralizado de faturamento para cliente
    ELSEIF NOT b_kna1-faksd IS INITIAL.
      MOVE: TEXT-001 TO tg_msg_ret-msg. APPEND tg_msg_ret. CLEAR: tg_msg_ret.
*  Bloqueio de contatos central para cliente
    ELSEIF NOT b_kna1-cassd IS INITIAL.
      MOVE: TEXT-001 TO tg_msg_ret-msg. APPEND tg_msg_ret. CLEAR: tg_msg_ret.
    ENDIF.

    SELECT SINGLE *
      FROM knb1
        INTO b_knb1
          WHERE kunnr EQ wg_header-kunnr
            AND bukrs EQ wg_header-vkorg.

*  Bloqueio Especifico parea uma empresas
    IF     NOT b_knb1-sperr IS INITIAL.
      MOVE: TEXT-001 TO tg_msg_ret-msg. APPEND tg_msg_ret. CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.
*    Fim Bloqueio cliente com restrições CS2016000357 WB

  DATA(cont) = REDUCE int4( INIT x = 0 FOR ls IN tg_itens WHERE ( vbeln IS NOT INITIAL ) NEXT x = x + 1 ).

  IF cont IS INITIAL.
    IF wg_header-tpsim EQ 'TS' OR wg_header-tpsim EQ 'TV'.
      IF wg_acao NE 'MODIF' AND wg_acao NE 'ADD'.
        PERFORM vlr_ajuste.
        IF vlr_total NE vta_sistpro.
          APPEND VALUE #( msg = TEXT-e50 ) TO tg_msg_ret.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*    Busca erro de imposto
  IF sy-ucomm EQ c_aprov.
    PERFORM verifica_erros_impostos.

    IF cont IS INITIAL.
      IF wg_header-tpsim EQ 'TS' OR wg_header-tpsim EQ 'TV'.
        PERFORM vlr_ajuste.
        IF vlr_total NE vta_sistpro.
          APPEND VALUE #( msg = TEXT-e50 ) TO tg_msg_ret.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.

  " 21.02.2023 - RAMON - 102323 -->
  IF wg_header-ecommerce = 'X'.

    DATA lv_msgn TYPE string.

    IF wg_header-tpsim NE 'VV' AND wg_header-tpsim NE 'BN'.

      CLEAR: tg_msg_ret.

      IF wg_header-tpsim IS INITIAL.
        lv_msgn = `Para Vendas E-commerce a ''Condição de Pagamento'' informada não é permitida`.
      ELSE.
        lv_msgn = `Para Vendas E-commerce a (` && wg_header-tpsim && `) informada não é permitida!`.
      ENDIF.

      MOVE lv_msgn TO tg_msg_ret-msg. APPEND tg_msg_ret.

    ENDIF.

    IF wg_header-id_order_ecommerce IS INITIAL.
      lv_msgn = `Para Vendas E-commerce o ''Cód. Pedido E-commerce'' deve ser informado`.

      MOVE lv_msgn TO tg_msg_ret-msg. APPEND tg_msg_ret.

    ENDIF.

    IF wg_header-id_order_ecommerce CO '0'.
      lv_msgn = `''Cód. Pedido E-commerce'' deve ser diferente de ZERO`.

      MOVE lv_msgn TO tg_msg_ret-msg. APPEND tg_msg_ret.

    ENDIF.

    IF wg_header-vendedor <> 'E00'.

      CLEAR: tg_msg_ret.

      lv_msgn = `Para Vendas E-commerce código do Vendedor deve ser 'E00 - E-COMMERCE'`.

      MOVE lv_msgn TO tg_msg_ret-msg. APPEND tg_msg_ret.

    ENDIF.

    IF wg_header-waerk <> 'BRL'.

      CLEAR: tg_msg_ret.

      lv_msgn = `Para Vendas E-commerce a Moeda deve ser BRL`.

      MOVE lv_msgn TO tg_msg_ret-msg. APPEND tg_msg_ret.

    ENDIF.

    IF wg_header-hbkid IS NOT INITIAL AND wg_header-hbkid NE 'AL5'.

      CLEAR: tg_msg_ret.

      lv_msgn = `Para Vendas E-commerce o banco empresa deve ser AL5`.

      MOVE lv_msgn TO tg_msg_ret-msg. APPEND tg_msg_ret.

    ENDIF.

    IF wg_header-hbkid IS INITIAL .

      CLEAR: tg_msg_ret.

      lv_msgn = 'Preencher Banco Empresa'.

      tg_msg_ret-field = 'WG_HEADER-HBKID'.

      MOVE lv_msgn TO tg_msg_ret-msg. APPEND tg_msg_ret.

    ENDIF.

  ENDIF.

  " 21.02.2023 - RAMON - 102323 --<

  SORT tg_msg_ret.
  DELETE ADJACENT DUPLICATES FROM tg_msg_ret COMPARING ALL FIELDS.

ENDFORM.                    " VERIFICA_ERROS

*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_dados .
  DATA: wl_tvbvk TYPE tvbvk,
        wl_tvkgr TYPE tvkgr.
*        WL_TVKBZ TYPE TVKBZ.

  CLEAR: wl_tvbvk, wl_tvkgr, wg_desc_kunnr, wg_header-fazenda,
*Início - Sara Oikawa - 38859 - Agosto/2020
         wg_desc_emissor, wg_desc_ort01, wg_desc_regio,
*Fim - Sara Oikawa - 38859 - Agosto/2020
* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
         wg_cnpj, wg_cpf,
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
         wg_desc_vkbur, wg_desc_waerk, wg_desc_cultura, wg_desc_tpcult.

  FREE: tg_setleaf_cult, tg_setlinet_cult.

  PERFORM f_check_date_retroativa.

  SELECT SINGLE name1 ort02
*Início - Sara Oikawa - 38859 - Agosto/2020
                ort01 regio
*Fim - Sara Oikawa - 38859 - Agosto/2020
* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
                stcd1 stcd2
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
    FROM kna1
    INTO (wg_desc_kunnr, wg_header-fazenda, wg_desc_ort01, wg_desc_regio,
* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
          wg_cnpj, wg_cpf)
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
     WHERE kunnr EQ wg_header-kunnr.

*Início - Sara Oikawa - 38859 - Agosto/2020
  IF NOT wg_desc_kunnr IS INITIAL.
    CONCATENATE wg_desc_kunnr '-' wg_desc_ort01 '-' wg_desc_regio INTO wg_desc_emissor SEPARATED BY space.
  ENDIF.
*Fim - Sara Oikawa - 38859 - Agosto/2020

  PERFORM zf_determine_funrural USING abap_false. " Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  SELECT SINGLE name1
    FROM t001w
    INTO wg_desc_vkbur
     WHERE werks EQ wg_header-vkbur.

  SELECT SINGLE *
    FROM t001k
      INTO wa_t001k
        WHERE bwkey EQ wg_header-vkbur.

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077
  CLEAR wg_desc_vendedor.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077
  SELECT SINGLE *
    FROM tvbvk
    INTO wl_tvbvk
     WHERE vkbur EQ wg_header-vkbur
       AND vkgrp EQ wg_header-vendedor.

  IF sy-subrc IS INITIAL.

    SELECT SINGLE bezei
      FROM tvgrt
      INTO wg_desc_vendedor
       WHERE spras EQ sy-langu
         AND vkgrp EQ wg_header-vendedor.

  ENDIF.

  SELECT SINGLE vkorg vtweg
    FROM tvkbz
    INTO (wg_header-vkorg, wg_header-vtweg)
     WHERE vkbur EQ wg_header-vkbur.

*** US - 81799 - CBRAND
  CLEAR: wg_desc_hbkid.
  SELECT SINGLE text1
    FROM t012t
    INTO wg_desc_hbkid
    WHERE spras EQ sy-langu
    AND bukrs EQ wg_header-vkorg
    AND hbkid EQ wg_header-hbkid.
*** US - 81799 - CBRAND

  SELECT SINGLE ktext
    FROM tcurt
    INTO wg_desc_waerk
     WHERE spras EQ sy-langu
       AND waers EQ wg_header-waerk.

  SELECT SINGLE descricao
    FROM zsdt0038
    INTO wg_desc_cultura
     WHERE cultura EQ wg_header-cultura.

  SELECT SINGLE *
    FROM setleaf
     INTO tg_setleaf_cult
     WHERE setname EQ 'MAGGI_ZSDT0044_03'
       AND valfrom EQ wg_header-tpcult.

  SELECT SINGLE descript
    FROM setlinet
    INTO wg_desc_tpcult
     WHERE setname EQ tg_setleaf_cult-setname
       AND langu   EQ sy-langu
       AND lineid  EQ tg_setleaf_cult-lineid.

  IF wg_desc_cultura IS NOT INITIAL.
    CONCATENATE 'Dt.Pagamento' wg_desc_cultura INTO wg_desc_dat SEPARATED BY space.
  ELSE.
    wg_desc_dat = 'Dt.Pagamento'.
  ENDIF.

  IF wg_header-tpsim EQ 'VP'.
    wg_desc_dat = 'Data Vencimento'.
  ENDIF.

** Verkaufsorganisationstext ermitteln
  PERFORM tvkot_select   USING wg_header-vkorg
                             space
                             sy-subrc.
** Vertriebsweg-Text ermitteln
  PERFORM tvtwt_select   USING wg_header-vtweg
                             space
                             sy-subrc.
*** Spartentext ermitteln
  PERFORM tspat_select   USING wg_header-spart
                             space
                             sy-subrc.



ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIME_SIMULACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WG_HEADER_SIMULACAO  text
*----------------------------------------------------------------------*
FORM imprime_simulacao  USING    p_simulacao.
  DATA: vl_form   TYPE tdsfname,
        vl_name   TYPE rs38l_fnam,
        wl_header TYPE zsds003,
        wl_kna1   TYPE kna1,
        wl_0040   TYPE zsdt0040,
        tl_itens  TYPE TABLE OF zsdt0041 WITH HEADER LINE.

  CLEAR: wl_kna1, wl_header.
  REFRESH: tl_itens.

  SELECT SINGLE *
    FROM zsdt0040
    INTO wl_0040
    WHERE doc_simulacao EQ wg_header-doc_simulacao.

  IF sy-subrc IS INITIAL.
    SELECT SINGLE vtext
      FROM tvkot
      INTO wl_header-org_vendas
       WHERE vkorg EQ wg_header-vkorg
         AND spras EQ sy-langu.

    SELECT SINGLE vtext
      FROM tvtwt
      INTO wl_header-canal_distr
       WHERE vtweg EQ wg_header-vtweg
         AND spras EQ sy-langu.

    SELECT SINGLE bezei
    FROM tvkbt
    INTO wl_header-escrit_vendas
     WHERE vkbur EQ wg_header-vkbur
       AND spras EQ sy-langu.

    SELECT SINGLE descricao
      FROM zsdt0038
      INTO wl_header-cultura
       WHERE cultura EQ wg_header-cultura.

    SELECT SINGLE *
      FROM kna1
      INTO wl_kna1
       WHERE kunnr EQ wg_header-kunnr.

    MOVE: wl_kna1-name1           TO wl_header-emissor,
          wl_kna1-stras           TO wl_header-endereco,
          wl_kna1-ort01           TO wl_header-municipio,
          wl_kna1-stcd1           TO wl_header-cnpj,
          wl_kna1-stcd2           TO wl_header-cpf,
          wl_kna1-stcd3           TO wl_header-ie,
          wl_kna1-regio           TO wl_header-uf,
          wl_kna1-pstlz           TO wl_header-cep,
          wg_area_ha              TO wl_header-area,
          wg_header-doc_simulacao TO wl_header-simulacao,
          wg_header-vkbur         TO wl_header-vkbur,
          wg_header-safra         TO wl_header-safra,
          wg_header-prec_ant_cult TO wl_header-preco,
          wg_header-doc_simulacao TO wl_header-cotacao,
          wg_header-fazenda       TO wl_header-fazenda,
          wg_header-juros_ano     TO wl_header-juros_ano,
          wg_header-vlr_adto      TO wl_header-vlr_adto,
          wg_header-adto_ha       TO wl_header-adto_ha,
          wg_header-area_penhor   TO wl_header-area_penhor,
          wg_tvkot-vtext          TO wl_header-org_vendas,
          wg_tvtwt-vtext          TO wl_header-canal_distr,
          wg_desc_vkbur           TO wl_header-escrit_vendas,
*          WG_HEADER-VENCI         TO WL_HEADER-VENCIMENTO,
          wg_header-trototsc      TO wl_header-totalsc.

    IF wg_header-tpsim EQ 'TS'.
      wl_header-modalidade = 'Troca Safra'.
      wl_header-tpsim   = wg_header-tpsim.
      wl_header-vencimento = wg_header-dtpgtcult.
    ELSEIF wg_header-tpsim EQ 'TV'.
      wl_header-modalidade = 'Troca a Vista'.
      wl_header-tpsim   = wg_header-tpsim.
      wl_header-vencimento = wg_header-dtpgtcult.
    ELSEIF wg_header-tpsim(1) EQ c_adt(1).
      wl_header-modalidade = 'Adiantamento'.
      wl_header-tpsim   = wg_header-tpsim.
    ELSEIF wg_header-tpsim EQ 'VV'.
      wl_header-modalidade = 'Venda a Vista'.
      wl_header-tpsim   = wg_header-tpsim.
    ELSEIF wg_header-tpsim EQ 'PM'.
      wl_header-modalidade = 'Permuta'.
      wl_header-tpsim   = wg_header-tpsim.
    ELSEIF wg_header-tpsim EQ 'VP'.
      wl_header-modalidade = 'Venda a Prazo'.
      wl_header-tpsim   = wg_header-tpsim.
    ELSEIF wg_header-tpsim EQ 'VF'.
      wl_header-modalidade = 'Venda Futura'.
      wl_header-tpsim   = wg_header-tpsim.
    ENDIF.

    LOOP AT tg_itens.
      MOVE: wg_header-doc_simulacao  TO tl_itens-doc_simulacao,
            tg_itens-posnr           TO tl_itens-posnr,
            tg_itens-matnr           TO tl_itens-matnr,
            tg_itens-zmeng           TO tl_itens-zmeng,
            tg_itens-zieme           TO tl_itens-zieme,
            tg_itens-zwert           TO tl_itens-zwert,
            tg_itens-calcu           TO tl_itens-calcu,
            tg_itens-trunit          TO tl_itens-trunit,
            tg_itens-siumb           TO tl_itens-siumb,
            tg_itens-trtot           TO tl_itens-trtot,
            tg_itens-compr           TO tl_itens-compr,
            tg_itens-vlrtot          TO tl_itens-vlrtot,
            tg_itens-auart           TO tl_itens-auart.

      APPEND tl_itens.
      CLEAR: tg_itens, tl_itens.
    ENDLOOP.

    SORT: tl_itens BY auart matnr.

    CALL  SELECTION-SCREEN 999 ENDING AT 51 8 STARTING AT 3 3.
    IF sy-subrc IS INITIAL.
      vl_form = 'ZSDS0002'.
*
      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = vl_form
        IMPORTING
          fm_name            = vl_name
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CALL FUNCTION vl_name
        EXPORTING
          wg_header        = wl_header
          wg_valor_unit    = p_vlr
        TABLES
          tg_itens         = tl_itens
        EXCEPTIONS
          formatting_error = 1
          internal_error   = 2
          send_error       = 3
          user_canceled    = 4
          OTHERS           = 5.

      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'O Doc. simulação não'
                                           'foi encontrado.'.
  ENDIF.
ENDFORM.                    " IMPRIME_SIMULACAO
*&---------------------------------------------------------------------*
*&      Form  TVKOT_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VBAK_VKORG  text
*      -->P_SPACE  text
*      -->P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM tvkot_select  USING us_vkorg
                         us_error
                         ch_subrc.

  ch_subrc = 0.
  CHECK: us_vkorg NE wg_tvkot-vkorg.

  IF us_vkorg = space.
    CLEAR: wg_tvkot.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM tvkot INTO wg_tvkot WHERE spras = sy-langu
                                             AND vkorg = us_vkorg.
  IF sy-subrc NE 0.
    CLEAR: wg_tvkot.
    IF us_error NE space.
      MESSAGE e313(v1) WITH us_vkorg.
    ELSE.
      ch_subrc = 4.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " TVKOT_SELECT
*---------------------------------------------------------------------*
*       FORM TSPAT_SELECT                                             *
*---------------------------------------------------------------------*
*       Sparte lesen Text                                             *
*---------------------------------------------------------------------*
FORM tspat_select USING us_spart
                        us_error
                        ch_subrc.

  ch_subrc = 0.
  CHECK: us_spart NE wg_tspat-spart.

  IF us_spart = space.
    CLEAR: wg_tspat.
    EXIT.
  ENDIF.


  SELECT SINGLE * FROM tspat INTO wg_tspat WHERE spras = sy-langu
                                             AND spart = us_spart.
  IF sy-subrc NE 0.
    CLEAR: wg_tspat.
    IF us_error NE space.
      MESSAGE e157(v2) WITH us_spart sy-langu.
    ELSE.
      ch_subrc = 4.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    "TSPAT_SELECT
*---------------------------------------------------------------------*
*       FORM TVTWt_SELECT                                             *
*---------------------------------------------------------------------*
*       Vertriebsweg und zugehörigen Text lesen                       *
*---------------------------------------------------------------------*
FORM tvtwt_select USING us_vtweg
                        us_error
                        ch_subrc.

  ch_subrc = 0.
  CHECK: us_vtweg NE wg_tvtwt-vtweg.

  IF us_vtweg = space.
    CLEAR: wg_tvtw, wg_tvtwt.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM tvtwt INTO wg_tvtwt WHERE spras = sy-langu
                                             AND vtweg = us_vtweg.
  IF sy-subrc NE 0.
    CLEAR: wg_tvtwt.
    IF us_error NE space.
      MESSAGE e314(v1) WITH us_vtweg.
    ELSE.
      ch_subrc = 4.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    "TVTWT_SELECT
*&---------------------------------------------------------------------*
*&      Module  ATUALIZA_DADOS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atualiza_dados INPUT.
  CLEAR wg_header-meio_pago.
*Início - Sara Oikawa - 38859 - Agosto/2020
  CLEAR wg_meio_pago.
*Fim - Sara Oikawa - 38859 - Agosto/2020
  PERFORM busca_dados.

ENDMODULE.                 " ATUALIZA_DADOS  INPUT
*&---------------------------------------------------------------------*
*&      Module  REFRESH_VENDOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE refresh_vkbur INPUT.
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077
*  CLEAR: wg_header-vendedor, wg_desc_vendedor, wg_header-vtweg,
*         wg_header-vkorg.
  CLEAR: wg_header-vtweg, wg_header-vkorg.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077
ENDMODULE.                 " REFRESH_VENDOR  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_VENDOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_vendor INPUT.
  DATA: tl_tvbvk TYPE TABLE OF tvbvk WITH HEADER LINE.
*        tl_tvgrt type table of tvgrt with header line.

  DATA: BEGIN OF tl_tvgrt OCCURS 0,
          vkgrp TYPE tvgrt-vkgrp,
          bezei TYPE tvgrt-bezei,
        END OF tl_tvgrt.

  CLEAR: tl_tvbvk, tl_tvgrt, tl_return_tab, tl_dselc.
  REFRESH: tl_tvbvk, tl_tvgrt, tl_return_tab, tl_dselc.

  SELECT *
    FROM tvbvk
    INTO TABLE tl_tvbvk
     WHERE vkbur EQ wg_header-vkbur.

  IF sy-subrc IS INITIAL.
    SELECT vkgrp bezei
      FROM tvgrt
      INTO TABLE tl_tvgrt
      FOR ALL ENTRIES IN tl_tvbvk
       WHERE spras EQ sy-langu
         AND vkgrp EQ tl_tvbvk-vkgrp.

  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VKGRP'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'WG_HEADER-VENDEDOR'
      value_org       = 'S'
    TABLES
      value_tab       = tl_tvgrt
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

ENDMODULE.                 " SEARCH_VENDOR  INPUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_DROPDOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_dropdown .
  DATA: ls_dropdown TYPE lvc_s_drop,
        lt_dropdown TYPE lvc_t_drop.


  ls_dropdown-handle = '1'.
  REFRESH: tg_setleaf , tg_setlinet, gt_values.

  ls_dropdown-handle = '2'.

  ls_dropdown-value = gt_values-ddtext = 'FOB'.
  gt_values-domvalue_l = 'FOB'.
  APPEND: ls_dropdown TO lt_dropdown,
          gt_values.

  ls_dropdown-value = gt_values-ddtext = 'CPT'.
  gt_values-domvalue_l = 'CPT'.
  APPEND: ls_dropdown TO lt_dropdown,
          gt_values.

  ls_dropdown-value = gt_values-ddtext = 'CIF'.
  gt_values-domvalue_l = 'CIF'.
  APPEND: ls_dropdown TO lt_dropdown,
          gt_values.

  ls_dropdown-value = gt_values-ddtext = 'CFR'.
  gt_values-domvalue_l = 'CFR'.
  APPEND: ls_dropdown TO lt_dropdown,
          gt_values.

* Übergabe der Dropdown-Tabelle an ALV-Grid-Control
  CALL METHOD grid1->set_drop_down_table
    EXPORTING
      it_drop_down = lt_dropdown.
ENDFORM.                    " BUILD_DROPDOWN
*&---------------------------------------------------------------------*
*&      Module  REFRESH_CALCU  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE refresh_calcu INPUT.
  PERFORM calcula_itens.
ENDMODULE.                 " REFRESH_CALCU  INPUT
*&---------------------------------------------------------------------*
*&      Form  CALCULA_ITENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calcula_itens.

  DATA: tl_0042         TYPE TABLE OF zsdt0042 WITH HEADER LINE,
        tl_0037         TYPE TABLE OF zsdt0037 WITH HEADER LINE,
        tl_0036         TYPE TABLE OF zsdt0036 WITH HEADER LINE,
        tl_itens        LIKE TABLE OF tg_itens,
        wl_itens        LIKE LINE OF tg_itens,
        wl_0039         TYPE zsdt0039,
        wl_vlr_perc     TYPE zsdt0041-zwert,
        wl_vlr_aliq     TYPE zsdt0042-vlr_aliq,
        wl_prunit       TYPE zsdt0041-zwert,
        wl_tot_vlr      TYPE zsdt0041-zwert,
        wl_tot_vlr_unit TYPE zsdt0041-zwert,
        wl_tabix        TYPE sy-tabix,
        wl_calc_aux     TYPE zsdt0041-calcu,
        wl_kna1         TYPE kna1,
        wl_valor_aux    LIKE wg_header-trototsc,
        wl_matkl        TYPE mara-matkl,
        wl_trototsc(30).

  CLEAR: wl_itens, wl_0039, wl_vlr_perc,
         wl_vlr_aliq, wl_tot_vlr_unit,
         wl_tabix, wl_kna1, wl_trototsc,
         wl_valor_aux, tl_0036, tl_0042,
         tl_0037, tl_itens, wl_prunit,
         wl_tot_vlr.

  SELECT SINGLE *
    FROM kna1
    INTO wl_kna1
   WHERE kunnr EQ wg_header-kunnr.

  IF wg_header-tpsim EQ 'TS' OR wg_header-tpsim EQ 'TV'.

    SELECT *
      FROM zsdt0042 INTO TABLE tl_0042
     WHERE cultura EQ wg_header-cultura
       AND waerk   EQ wg_header-waerk
       AND estado  EQ wl_kna1-regio
       AND safra   EQ wg_header-safra  " RJF - 61516 - 2023.08.31
       AND val_de  LE wg_header-dtent  " RJF - 61516 - 2023.08.31
       AND val_ate GE wg_header-dtent. " RJF - 61516 - 2023.08.31

    LOOP AT tl_0042.
     CHECK ( tl_0042-witht NE 'FI' ).
      IF ( tl_0042-witht EQ 'FR' ).
        IF ( wl_kna1-stkzn IS NOT INITIAL ).
          IF wg_header-funrural IS INITIAL.
            ADD tl_0042-vlr_perc TO wl_vlr_perc.
          ELSE.
            ADD tl_0042-vlr_perc1 TO wl_vlr_perc.
          ENDIF.
          ADD tl_0042-vlr_aliq TO wl_vlr_aliq.
        ENDIF.
      ELSE.
        ADD tl_0042-vlr_perc TO wl_vlr_perc.
        ADD tl_0042-vlr_aliq TO wl_vlr_aliq.
      ENDIF.
        ENDLOOP.

  ENDIF.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  IF wg_acao EQ c_atual AND sy-ucomm NE 'BTN_FUN'.
    wl_vlr_perc = wg_header-inss.
    wl_vlr_aliq = wg_header-facs.
  ENDIF.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  wg_header-inss = wl_vlr_perc.
  wg_header-facs = wl_vlr_aliq.

  IF ( tg_itens[] IS NOT INITIAL ).

    SELECT *
      FROM zsdt0037
INTO TABLE tl_0037
   FOR ALL ENTRIES IN tg_itens
     WHERE val_de          LE sy-datum
       AND val_ate         GE sy-datum
       AND meins           EQ tg_itens-zieme
       AND filial_origem   EQ tg_itens-werks
       AND filial_destino  EQ wg_header-vkbur
       AND waers           EQ 'BRL'
       AND bukrs           EQ wa_t001k-bukrs.

    SELECT *
      FROM zsdt0036
INTO TABLE tl_0036
   FOR ALL ENTRIES IN tg_itens
     WHERE val_de       LE sy-datum
       AND val_ate      GE sy-datum
       AND matnr        EQ tg_itens-matnr
       AND bukrs        EQ wa_t001k-bukrs
       AND waerk        EQ wg_header-waerk
       AND meins        EQ tg_itens-zieme
       AND safra        EQ wg_header-safra
       AND inco1        EQ tg_itens-inco1
       AND werks_fornec EQ tg_itens-werks
       AND loekz        EQ space
       AND cultura      IN (wg_header-cultura , space )
       AND eliminado    EQ space.

*    IF WG_HEADER-WAERK EQ 'USD'.
*      CLEAR TAXA.
*
*      SELECT SINGLE KURSK
*        FROM ZSDT0117
*        INTO TAXA_FRE
*        WHERE BUKRS EQ WA_T001K-BUKRS
*        AND DESATIVADO NE ABAP_TRUE.
*
*      WL_0037-VLR_FRETE = WL_0037-VLR_FRETE / TAXA.
*    ENDIF.

  ENDIF.

  LOOP AT tg_itens INTO wl_itens.
    wl_tabix = sy-tabix.

    CLEAR: tl_0036.
    READ TABLE tl_0036 WITH KEY matnr = wl_itens-matnr
                                inco1 = wl_itens-inco1
                              cultura = wg_header-cultura
                         werks_fornec = wl_itens-werks.

    IF ( sy-subrc IS NOT INITIAL ).
      READ TABLE tl_0036 WITH KEY matnr = wl_itens-matnr
                                  inco1 = wl_itens-inco1
                                cultura = space
                           werks_fornec = wl_itens-werks.

    ENDIF.
*** CALCULO CIF
    IF ( wl_itens-inco1 EQ 'CIF' ).
      CLEAR  tl_0037.
      SELECT SINGLE matkl
        FROM mara
        INTO wl_matkl
       WHERE matnr EQ wl_itens-matnr.

      READ TABLE tl_0037 WITH KEY filial_origem  = wl_itens-werks
                                          meins  = wl_itens-zieme
                                  filial_destino = wg_header-vkbur
                                          waers  = 'BRL'
                                          matkl  = wl_matkl.

      PERFORM atualiza_frete CHANGING tl_0037-vlr_frete.
    ELSE.
      CLEAR: tl_0037.
    ENDIF.

    MOVE: tl_0036-perc_margem  TO wl_itens-mgcad.
    DATA: lv_value_aux TYPE f.

    IF NOT tl_0036-vlr_venda IS INITIAL AND
       NOT wl_itens-zwert IS INITIAL.
*       NOT TL_0037-VLR_FRETE IS INITIAL.
      TRY.                                                                                           "BUG 46674 - 28.10.2020
          lv_value_aux  =  ( ( tl_0036-vlr_venda / ( wl_itens-zwert - tl_0037-vlr_frete ) ) - 1 ) * 100.
        CATCH  cx_sy_zerodivide.
      ENDTRY.
    ENDIF.

**  Realiza o calculo do campo  "Vlr Negociado"  **
    IF ( wl_itens-desconto NE 0 ).

*     WL_CALC_AUX    = WL_ITENS-DESCONTO / 100.
      wl_calc_aux    = lv_value_aux / 100.
      ADD 1 TO wl_calc_aux.

      wl_itens-zwert = ( tl_0036-vlr_venda / wl_calc_aux ).
      ADD tl_0037-vlr_frete TO wl_itens-zwert.

    ELSE.
      wl_itens-zwert = ( tl_0036-vlr_venda + tl_0037-vlr_frete ).
    ENDIF.

**  Realiza o calculo do campo "Margem Efetiva" **
    TRY.
        wl_itens-mgefe = ( ( ( wl_itens-zwert - tl_0036-vlr_custo ) / wl_itens-zwert ) * 100 ).
      CATCH: cx_sy_zerodivide, cx_sy_arithmetic_overflow.
        wl_itens-mgefe = 0.
    ENDTRY.

**  Realiza calculo do "Vlr unitário"
    wl_itens-vl_unit = ( tl_0036-vlr_venda + tl_0037-vlr_frete ).

    IF ( wg_header-tpsim(1) EQ c_tro(1) ).
      wl_itens-calcu = wl_itens-zwert.
**    Realiza o calculo do campo "Troca Unitaria"

      TRY.
          wl_prunit       = wg_header-prec_ant_cult. " / ( ( WG_HEADER-ANTEC / 100 ) + 1 ).
          IF NOT wl_prunit IS INITIAL.
            wl_itens-trunit = ( wl_itens-calcu / ( ( wl_prunit - ( ( wl_prunit * ( wl_vlr_perc / 100 ) ) + ( wl_vlr_aliq ) ) ) ) ).
          ENDIF.
        CATCH cx_sy_zerodivide.
      ENDTRY.

**    Realiza o calculo do campo "Troca Total Item" **
      wl_itens-trtot = wl_itens-trunit * wl_itens-zmeng.

    ELSEIF ( wg_header-tpsim(1) EQ c_adt(1) ).
**    Realiza o calculo do campo "Compromisso"
      TRY.
          wl_itens-compr = wl_itens-vlrtot / wg_header-vlr_adto.
        CATCH cx_sy_zerodivide.
      ENDTRY.

    ELSEIF ( wg_header-tpsim(2) EQ 'BN' ).

      CLEAR: wl_itens-zwert, wl_itens-vlrtot, wl_calc_aux.
      wl_itens-zwert = ( tl_0036-vlr_custo ).

    ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
    IF wg_header-tpsim(2) NE 'VP'.
*Fim - Sara Oikawa - 38859 - Agosto/2020
      "JUROS
      wl_dias_vct = 0.
      IF ( NOT wg_header-dtinijuros IS INITIAL ).
        wl_dias_vct = wg_header-dtpgtcult - wg_header-dtinijuros.
      ENDIF.

      IF ( wl_dias_vct GT 0 ).
        wl_juro_dias     = ( ( wg_header-juros_ano / 365 ) * wl_dias_vct ) / 100.
        wl_itens-vl_unit = wl_itens-zwert = wl_itens-zwert + ( wl_itens-zwert * wl_juro_dias ).
      ENDIF.
*Início - Sara Oikawa - 38859 - Agosto/2020
    ENDIF.
*Fim - Sara Oikawa - 38859 - Agosto/2020

*   Realiza o calculo do campo "Valor Total item"
    wl_itens-vlrtot = wl_itens-zwert * wl_itens-zmeng.

*   Preenche o imposto
    PERFORM busca_imposto CHANGING wl_itens.

    MODIFY tg_itens FROM wl_itens INDEX wl_tabix.

  ENDLOOP.

***** Método de atualização de dados na Tela
  CALL METHOD grid1->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CLEAR: wl_itens.
  IF ( wg_header-tpsim(1) EQ c_adt(1) ).
    CLEAR: wl_tot_vlr,
           wg_header-vlrtot,
           wg_header-comprsc.

    PERFORM vlrtot.

    LOOP AT tg_itens INTO wl_itens.
      ADD wl_itens-vlrtot TO wl_tot_vlr.
**    Realiza o calculo do campo "Valor Total"
*      ADD WL_ITENS-VLRTOT TO WG_HEADER-VLRTOT.
**    Realiza o calculo do campo "Compromisso/sac"
      ADD wl_itens-compr  TO wg_header-comprsc.
    ENDLOOP.
**    Realiza o calculo do campo "Area de Penhor"
    TRY.
        wg_header-area_penhor = wl_tot_vlr / wg_header-adto_ha.
      CATCH cx_sy_zerodivide.
    ENDTRY.

  ELSEIF wg_header-tpsim(1) EQ c_tro(1).
    CLEAR: wg_header-trototsc, wg_header-scha, wg_header-vlrtot, convert_tratotsc.

    PERFORM vlrtot.

    LOOP AT tg_itens INTO wl_itens.
*    Realiza o calculo do campo "Troca Total Sc"
      ADD: wl_itens-trtot TO wg_header-trototsc,
           wl_itens-trtot TO convert_tratotsc.
**    Realiza o calculo do campo "Valor Total"
*      ADD WL_ITENS-VLRTOT TO WG_HEADER-VLRTOT.
    ENDLOOP.

**    Realiza o calculo do campo "Sc/há"
    TRY.
        wg_header-scha = wg_header-trototsc / wg_area_ha. "WG_HEADER-AREA_HA.
      CATCH cx_sy_zerodivide.
    ENDTRY.

  ENDIF.
ENDFORM.                    " CALCULA_ITENS
*&---------------------------------------------------------------------*
*&      Module  SEARCH_SAFRA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_safra INPUT.
  DATA: BEGIN OF tl_safra OCCURS 0,
          safra     TYPE zsdt0044-safra,
          descricao TYPE zsdt0044-descricao,
        END OF tl_safra.

  REFRESH: tl_safra, tl_return_tab, tl_dselc.
  CLEAR: tl_safra, tl_return_tab, tl_dselc.

  SELECT safra descricao
    FROM zsdt0044
    INTO TABLE tl_safra.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'SAFRA'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'WG_HEADER-SAFRA'
      value_org       = 'S'
    TABLES
      value_tab       = tl_safra
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " SEARCH_SAFRA  INPUT

*&      Form  MODIFICA_STATUS
FORM modifica_status  USING  p_status.

  DATA: wl_0040 TYPE zsdt0040.

  CLEAR: wl_0040.

  IF p_status EQ c_b.

    DATA: p_resp.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question         = 'Deseja Bloqueiar este Simulador?               Após Bloqueado o mesmo ficará disponível apenas para Visualização!'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        display_cancel_button = ' '
      IMPORTING
        answer                = p_resp.

    CHECK p_resp EQ 1.

  ENDIF.

  DATA(doc) = wg_header-doc_simulacao.
  doc = |{ doc ALPHA = OUT }|.

*// Bloqueia o Documento de Simulação para que o JOB não execute
*  CALL FUNCTION 'ENQUEUE_EZSDT0040_JOB'
*    EXPORTING
*      DOC_SIMULACAO  = DOC
*    EXCEPTIONS
*      FOREIGN_LOCK   = 1
*      SYSTEM_FAILURE = 2
*      OTHERS         = 3.


  SELECT SINGLE *
    FROM zsdt0040
    INTO wl_0040
     WHERE doc_simulacao EQ wg_header-doc_simulacao.

  IF sy-subrc IS INITIAL.

*    Realiza o desbloqueio das ação enteriores e bloqueia novamente para não acumular Bloqueio.
    CALL FUNCTION 'DEQUEUE_EZSDT0040'
      EXPORTING
        doc_simulacao = wg_header-doc_simulacao.

    CALL FUNCTION 'ENQUEUE_EZSDT0040'
      EXPORTING
        doc_simulacao  = wg_header-doc_simulacao
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " INICIO LOG AÇÕES
    PERFORM log_acao USING p_status '' CHANGING lv_erro.
    " FIM LOG AÇÕES

    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
    CHECK lv_erro IS INITIAL.
    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<


    MOVE: p_status TO wl_0040-status.

    "CS2021000667 - #111424 - SC -->
*    CASE wl_0040-status.
*      WHEN c_a. wg_status = icon_release.
*      WHEN c_r. wg_status = icon_defect.
*      WHEN c_b. wg_status = icon_gis_pan.
*      WHEN OTHERS.
*        IF wl_0040-status IS INITIAL.
*          wg_status = icon_initial.
*        ENDIF.
*    ENDCASE.
*
*    wl_0040-job = abap_false.



    "MODIFY zsdt0040 FROM wl_0040.
    "CS2021000667 - #111424 - SC --<

    " 19.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- >
    TRY .

*    IF SY-UNAME NE 'WBARBOSA'.
        CASE p_status.
          WHEN c_a.

*    DISPARO DO HEDGE PARA VENDA
            IF wl_0040-waerk EQ 'BRL'.

              CASE wl_0040-tpsim.
                WHEN 'BN' OR 'PM'.
                WHEN OTHERS.
                  zcl_webservice_tx_curva=>hedge_insumos( i_numero = wg_header-doc_simulacao
                                                          i_acao   = sy-ucomm
                                                          i_tipo   = 'VDI'
                                                          " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
                                                          i_taxa_boleta = gv_taxa_neg
                                                          " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<
                                                         ).
              ENDCASE.
            ENDIF.

*    DISPARO DO HEDGE PARA FRETE
            IF wl_0040-tpsim NE 'PM'.
              zcl_webservice_tx_curva=>hedge_insumos( i_numero = wg_header-doc_simulacao
                                                      i_acao   = sy-ucomm
                                                      i_tipo   = 'FRI'
                                                      " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
                                                      "i_taxa_boleta = gv_taxa_neg
                                                      " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<
                                                     ).
            ENDIF.

          WHEN c_r OR c_b.
*    REVERTE O DISPARO DO HEDGE PARA FRETE E VENDA
            zcl_webservice_tx_curva=>hedge_insumos( i_numero = wg_header-doc_simulacao
                                                    i_tipo   = 'EST'
                                                   ).
        ENDCASE.
*    ENDIF.

        CALL FUNCTION 'DEQUEUE_EZSDT0040'
          EXPORTING
            doc_simulacao = wg_header-doc_simulacao.


*    CALL FUNCTION 'DEQUEUE_EZSDT0040_JOB'
*      EXPORTING
*        DOC_SIMULACAO = DOC.

        CLEAR sy-ucomm.


      CATCH zcx_webservice.
        lv_erro = abap_true. "CS2021000667 - #111424 - SC"
        MESSAGE w104(zsd).
    ENDTRY.
    " 19.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- <

    "CS2021000667 - #111424 - SC -->
    IF lv_erro IS INITIAL.

      CASE wl_0040-status.
        WHEN c_a. wg_status = icon_release.
        WHEN c_r. wg_status = icon_defect.
        WHEN c_b. wg_status = icon_gis_pan.
        WHEN OTHERS.
          IF wl_0040-status IS INITIAL.
            wg_status = icon_initial.
          ENDIF.
      ENDCASE.

      wl_0040-job = abap_false.

      MODIFY zsdt0040 FROM wl_0040.

    ENDIF.
    "CS2021000667 - #111424 - SC --<

  ENDIF.

ENDFORM.                    " MODIFICA_STATUS
*&---------------------------------------------------------------------*
*&      Form  VALIDA_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UNAME  text
*      -->P_T_FIELDCATALOG  text
*----------------------------------------------------------------------*
FORM valida_layout  TABLES   tl_fieldcatalog STRUCTURE lvc_s_fcat
                     USING   uname.

*BREAK-POINT.

  DATA: tl_parametros        TYPE ustyp_t_parameters,
        wl_parametros        TYPE ustyp_parameters,
        wl_fieldcatalog      TYPE lvc_s_fcat,
        wl_variante01        TYPE zvariante01,
        tl_variante02_alv    TYPE TABLE OF zvariante02 WITH HEADER LINE,
        tl_variante02_screen TYPE TABLE OF zvariante02 WITH HEADER LINE,
        wl_tabix             TYPE sy-tabix,
        wl_atributo(30).

  REFRESH: tl_parametros, tl_variante02_alv, tl_variante02_screen.
  FIELD-SYMBOLS: <fs_atributos> TYPE any.

  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = uname
*     WITH_TEXT           =
    TABLES
      user_parameters     = tl_parametros
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
  ENDIF.

  READ TABLE tl_parametros INTO wl_parametros
    WITH KEY parid = 'ZVARIANTE'.
  IF sy-subrc IS INITIAL.
    SELECT SINGLE *
      FROM zvariante01
      INTO wl_variante01
       WHERE grpva EQ wl_parametros-parva
         AND tcode EQ sy-tcode.

    IF sy-subrc IS INITIAL.
      CONDENSE wl_variante01-grpva NO-GAPS.
      SELECT *
        FROM zvariante02
        INTO TABLE tl_variante02_alv
         WHERE grpva   EQ wl_variante01-grpva
           AND tcode   EQ sy-tcode
           AND atr_tip EQ 'ALV'
           AND dynnr   EQ sy-dynnr.

      SELECT *
        FROM zvariante02
        INTO TABLE tl_variante02_screen
         WHERE grpva   EQ wl_variante01-grpva
           AND tcode   EQ sy-tcode
           AND atr_tip NE 'ALV'
           AND dynnr   EQ sy-dynnr.

    ENDIF.
    IF tl_variante02_screen[] IS NOT INITIAL
    AND ( sy-tcode NE 'SE38'
       AND sy-tcode NE 'SE80' ).
      LOOP AT SCREEN.
        READ TABLE tl_variante02_screen
          WITH KEY field = screen-name.

        IF sy-subrc IS INITIAL.

*** Modificação - Eduardo Ruttkowski Tavares - 31.07.2013 >>> INI
*CH 103640  - Simulador de Vendas - Ajuste campo Taxa Antecipação
          IF tl_variante02_screen-acao IS NOT INITIAL  AND
             wg_acao                   IS NOT INITIAL.
*** Modificação - Eduardo Ruttkowski Tavares - 31.07.2013 <<< FIM
            IF ( tl_variante02_screen-acao IS NOT INITIAL
*** Modificação - Eduardo Ruttkowski Tavares - 31.07.2013 >>> INI
*CH 103640  - Simulador de Vendas - Ajuste campo Taxa Antecipação
*           AND TL_VARIANTE02_SCREEN-ACAO eq WG_ACAO )
            AND tl_variante02_screen-acao CS wg_acao )
*** Modificação - Eduardo Ruttkowski Tavares - 31.07.2013 <<< FIM
              OR tl_variante02_screen-acao IS INITIAL.
              UNASSIGN <fs_atributos>.
              CONCATENATE 'SCREEN' tl_variante02_screen-atr_tip INTO wl_atributo SEPARATED BY '-'.
              ASSIGN (wl_atributo) TO <fs_atributos>.
              IF <fs_atributos> IS ASSIGNED.
                <fs_atributos> = tl_variante02_screen-fatr_value.
                MODIFY SCREEN.
              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF tl_variante02_alv[] IS INITIAL
    AND ( sy-tcode EQ 'SE38'
       OR sy-tcode EQ 'SE80' ).
      EXIT.
    ENDIF.

    READ TABLE tl_variante02_alv WITH KEY dynnr = sy-dynnr.
    IF sy-subrc IS INITIAL.
      LOOP AT tl_fieldcatalog INTO wl_fieldcatalog.
        wl_tabix = sy-tabix.
        READ TABLE tl_variante02_alv WITH KEY field = wl_fieldcatalog-fieldname.
        IF sy-subrc IS NOT INITIAL.
          IF ( tl_variante02_screen-acao IS NOT INITIAL
           AND tl_variante02_screen-acao EQ wg_acao )
            OR tl_variante02_screen-acao IS INITIAL.
            DELETE tl_fieldcatalog INDEX wl_tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF sy-tabix EQ 1. ENDIF.
  ELSE.
    SELECT SINGLE *
      FROM zvariante01
      INTO wl_variante01
       WHERE default_var EQ c_x
         AND tcode EQ sy-tcode.

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM zvariante02
        INTO TABLE tl_variante02_alv
         WHERE grpva   EQ wl_variante01-grpva
           AND tcode   EQ sy-tcode
           AND atr_tip EQ 'ALV'
           AND dynnr   EQ sy-dynnr.

      SELECT *
         FROM zvariante02
         INTO TABLE tl_variante02_screen
          WHERE grpva   EQ wl_variante01-grpva
            AND tcode   EQ sy-tcode
            AND atr_tip NE 'ALV'
            AND dynnr   EQ sy-dynnr.
    ENDIF.
    IF tl_variante02_screen[] IS NOT INITIAL
        AND ( sy-tcode NE 'SE38'
           AND sy-tcode NE 'SE80' ).
      LOOP AT SCREEN.
        READ TABLE tl_variante02_screen
          WITH KEY field = screen-name.

        IF sy-subrc IS INITIAL.
          IF ( tl_variante02_screen-acao IS NOT INITIAL
            AND tl_variante02_screen-acao EQ wg_acao )
              OR tl_variante02_screen-acao IS INITIAL.
            UNASSIGN <fs_atributos>.
            CONCATENATE 'SCREEN' tl_variante02_screen-atr_tip INTO wl_atributo SEPARATED BY '-'.
            ASSIGN (wl_atributo) TO <fs_atributos>.
            IF <fs_atributos> IS ASSIGNED.
              <fs_atributos> = tl_variante02_screen-fatr_value.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF tl_variante02_alv[] IS INITIAL
    AND ( sy-tcode EQ 'SE38'
       OR sy-tcode EQ 'SE80' ).
      EXIT.
    ENDIF.
    READ TABLE tl_variante02_alv WITH KEY dynnr = sy-dynnr.
    IF sy-subrc IS INITIAL.
      LOOP AT tl_fieldcatalog INTO wl_fieldcatalog.
        wl_tabix = sy-tabix.
        READ TABLE tl_variante02_alv WITH KEY field = wl_fieldcatalog-fieldname.
        IF sy-subrc IS NOT  INITIAL.
          IF ( tl_variante02_alv-acao IS NOT INITIAL
           AND tl_variante02_alv-acao EQ wg_acao )
            OR tl_variante02_alv-acao IS INITIAL.
            DELETE tl_fieldcatalog INDEX wl_tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " VALIDA_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  MONTA_proposta
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM monta_proposta.
  DATA: tl_0042 TYPE TABLE OF zsdt0042 WITH HEADER LINE,
        wl_kna1 TYPE kna1.

  CLEAR: tl_0042, wl_kna1, wg_prop.
  REFRESH: tl_0042.

  SELECT SINGLE *
    FROM kna1
    INTO wl_kna1
     WHERE kunnr EQ wg_header-kunnr.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  IF wg_acao EQ c_atual.
    wg_prop-inss  = wg_header-inss.
    wg_prop-facs  = wg_header-facs.
    wg_prop-tante = wg_header-antec.        "Taxa de Antecipado
    wg_prop-fundeinfra = wg_header-fundeinfra.
  ELSE.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

    wg_prop-tante = wg_header-antec.        "Taxa de Antecipado

    IF wg_header-tpsim EQ 'TS' OR wg_header-tpsim EQ 'TV'.

      SELECT *
        FROM zsdt0042
        INTO TABLE tl_0042
         WHERE cultura EQ wg_header-cultura
           AND waerk   EQ wg_header-waerk
           AND estado  EQ wl_kna1-regio   " INCLUIDO REGIO DO CLIENTE
           AND safra   EQ wg_header-safra  " RJF - 61516 - 2023.08.31
           AND val_de  LE wg_header-dtent  " RJF - 61516 - 2023.08.31
           AND val_ate GE wg_header-dtent. " RJF - 61516 - 2023.08.31

      IF wl_kna1-stkzn IS NOT INITIAL.
*  * Taxa do INSS
        READ TABLE tl_0042 WITH KEY witht = 'FR'
                                    waerk = wg_header-waerk.
        IF sy-subrc IS INITIAL.
          IF wg_header-funrural IS INITIAL.
            wg_prop-inss = tl_0042-vlr_perc.
          ELSE.
            wg_prop-inss = tl_0042-vlr_perc1.
          ENDIF.
        ENDIF.

      ENDIF.
*#138092 -  ITSOUZA - Inicio
*  * Taxa do FUNDEINFRA
      READ TABLE tl_0042 WITH KEY witht = 'FI'
                                  waerk = wg_header-waerk.
      IF sy-subrc IS INITIAL.
        IF wg_header-fundeinfra_exce IS INITIAL.
          wg_prop-fundeinfra = tl_0042-vlr_perc.
        ELSE.
          wg_prop-fundeinfra = tl_0042-vlr_perc1.
        ENDIF.
      ENDIF.
*#138092 -  ITSOUZA - Fim

*  * Taxa do FACS/FETHAB
      READ TABLE tl_0042 WITH KEY witht = 'FT'
                                  waerk = wg_header-waerk.
      IF sy-subrc IS INITIAL.
        IF tl_0042-waerk EQ wg_header-waerk.
          MOVE  tl_0042-vlr_aliq TO wg_prop-facs.
        ENDIF.
      ENDIF.

    ENDIF.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  ENDIF.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  CLEAR wl_0040.

  SELECT SINGLE *
    FROM zsdt0040
    INTO wl_0040
   WHERE doc_simulacao EQ wg_header-doc_simulacao.

  IF ( wl_0040-prec_cult IS NOT INITIAL ).

    TRY.
        wg_prop-vlrant_aux = ( wl_0040-prec_cult / ( 1 + ( wg_prop-tante / 100 ) ) ) .
        wg_prop-vlrant = wg_prop-vlrant_aux.
      CATCH cx_sy_zerodivide.
    ENDTRY.

  ELSE.
    wg_prop-vlrant = wg_header-prec_ant_cult.
  ENDIF.

** Valor total Antecipado Bruto
  wg_prop-vta_bruto = convert_tratotsc * wg_prop-vlrant.


ENDFORM.                    "MONTA_proposta

*&---------------------------------------------------------------------*
*&      Form  MONTA_MEMO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM monta_memo.
  DATA: tl_0042 TYPE TABLE OF zsdt0042 WITH HEADER LINE,
        wl_kna1 TYPE kna1.

  CLEAR: tl_0042, wl_kna1, wg_memo, wl_0040.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  IF wg_acao EQ c_atual AND sy-ucomm NE 'BTN_FUN'.
    wg_memo-inss  = wg_header-inss.
    wg_memo-facs  = wg_header-facs.
    wg_memo-tante = wg_header-antec. "Taxa de Antecipado
  ELSE.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

    wg_memo-tante = wg_header-antec. "Taxa de Antecipado

    IF wg_header-tpsim EQ 'TS' OR wg_header-tpsim EQ 'TV'.

      SELECT SINGLE *
        FROM kna1
        INTO wl_kna1
       WHERE kunnr EQ wg_header-kunnr.

      SELECT *
        FROM zsdt0042
        INTO TABLE tl_0042
       WHERE cultura EQ wg_header-cultura
         AND waerk   EQ wg_header-waerk
           AND estado  EQ wl_kna1-regio
           AND safra   EQ wg_header-safra  " RJF - 61516 - 2023.08.31
           AND val_de  LE wg_header-dtent  " RJF - 61516 - 2023.08.31
           AND val_ate GE wg_header-dtent. " RJF - 61516 - 2023.08.31



*   __________________Taxa do INSS__________________________
      IF ( wl_kna1-stkzn IS NOT INITIAL ).
        READ TABLE tl_0042 WITH KEY witht = 'FR'
                                    waerk = wg_header-waerk.
        IF ( sy-subrc IS INITIAL ).
          wg_memo-inss = COND #( WHEN wg_header-funrural IS INITIAL THEN tl_0042-vlr_perc ELSE tl_0042-vlr_perc1 ).
        ENDIF.
      ENDIF.

*   _________________Taxa do FACS/FETHAB____________________
      READ TABLE tl_0042 WITH KEY witht = 'FT'
                                  waerk = wg_header-waerk.
      IF ( sy-subrc IS INITIAL ).
        IF tl_0042-waerk EQ wg_header-waerk.
          wg_memo-facs = tl_0042-vlr_aliq.
        ENDIF.
      ENDIF.
    ENDIF.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

    wg_header-inss        = wg_memo-inss.
    wg_header-facs        = wg_memo-facs.

  ENDIF.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6


*#138092 - ITSOUZA - Inicio
*  IF ( wg_acao EQ c_atual OR wg_acao EQ c_modif ) AND sy-ucomm NE 'BTN_FI' AND wg_copy NE abap_true.
   IF ( wg_acao EQ c_atual ) AND sy-ucomm NE 'BTN_FI' AND wg_copy NE abap_true."SMC
    wg_memo-fundeinfra = wg_header-fundeinfra.
  ELSE.
    IF wl_kna1 IS INITIAL AND
       tl_0042 IS INITIAL.

      SELECT SINGLE *
        FROM kna1
        INTO wl_kna1
       WHERE kunnr EQ wg_header-kunnr.

      SELECT *
        FROM zsdt0042
        INTO TABLE tl_0042
       WHERE cultura EQ wg_header-cultura
         AND waerk   EQ wg_header-waerk
           AND estado  EQ wl_kna1-regio
           AND safra   EQ wg_header-safra  " RJF - 61516 - 2023.08.31
           AND val_de  LE wg_header-dtent  " RJF - 61516 - 2023.08.31
           AND val_ate GE wg_header-dtent. " RJF - 61516 - 2023.08.31
    ENDIF.

*   __________________Taxa do fundeinfra__________________________
*    IF ( wl_kna1-stkzn IS NOT INITIAL ). Nesse caso tanto faz para pessoa fisica quanto para juridica - SMC
    READ TABLE tl_0042 WITH KEY witht = 'FI'
                                waerk = wg_header-waerk.

    IF ( sy-subrc IS INITIAL ).
*        IF sy-ucomm EQ 'BTN_FI'.
*          wg_header-fundeinfra_exce  = abap_true.
      wg_memo-fundeinfra = COND #( WHEN wg_header-fundeinfra_exce IS INITIAL THEN tl_0042-vlr_perc
                                   ELSE tl_0042-vlr_perc1 ).
*        ELSE.
*          wg_memo-fundeinfra = tl_0042-vlr_perc.
*        ENDIF.
    ENDIF.
*    ENDIF.

    wg_header-fundeinfra  = wg_memo-fundeinfra.
  ENDIF.
  CLEAR wg_copy.
*#138092 - ITSOUZA - Fim

* _________________Valor Antecipado Bruto________________

  SELECT SINGLE *
    FROM zsdt0040
    INTO wl_0040
   WHERE doc_simulacao EQ wg_header-doc_simulacao.

  IF ( wl_0040-prec_cult IS NOT INITIAL ).

    TRY.
        wg_memo-vlrant_aux = ( wl_0040-prec_cult / ( 1 + ( wg_memo-tante / 100 ) ) ) .
        wg_memo-vlrant = wg_memo-vlrant_aux.
      CATCH cx_sy_zerodivide.
    ENDTRY.

  ELSE.
    wg_memo-vlrant = wg_header-prec_ant_cult.
  ENDIF.

* ____________________Valor INSS________________________________________
  TRY.
      wg_memo-vlrinss_aux = ( wg_memo-vlrant * ( wg_memo-inss / 100 ) ).
      wg_memo-vlrinss     =   wg_memo-vlrinss_aux.
    CATCH cx_sy_zerodivide.
  ENDTRY.


*#138092 - ITSOUZA - Inicio
* ____________________Valor Fundeinfra________________________________________
  TRY.
      wg_memo-vlrfundein_aux = ( wg_memo-vlrant * ( wg_memo-fundeinfra / 100 ) ).
      wg_memo-vlrfundein     =   wg_memo-vlrfundein_aux.
    CATCH cx_sy_zerodivide.
  ENDTRY.
*#138092 - ITSOUZA - Fim

* __________________Valor FACS/FETHAB___________________________________

  wg_memo-vlrfacs    = wg_memo-facs.

* _________________Preço Sc. Liquido____________________________________

  wg_memo-pliqdo_aux = ( wg_memo-vlrant - ( wg_memo-vlrinss_aux + wg_memo-vlrfacs ) - wg_memo-vlrfundein ).
  wg_memo-pliqdo     = wg_memo-pliqdo_aux.

* _____________________________________________________________________


ENDFORM.                    " MONTA_MEMO
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.

  DATA: status_btn TYPE char1.

  SET PF-STATUS 'Z01'.
  SET TITLEBAR 'Z002'.

  CLEAR status_btn.

*#138092 -  ITSOUZA - 28.05.2024 15:59:54 - Inicio
  SELECT SINGLE *
    FROM zsdt0043
    INTO wl_0043
     WHERE werks EQ wl_0040-vkbur
       AND uname EQ sy-uname.

  SELECT SINGLE *
FROM kna1
INTO @DATA(wl_kna1)
WHERE kunnr EQ @wg_header-kunnr.

  SELECT *
    FROM zsdt0042
    INTO TABLE @DATA(lt_zsdt0042)
   WHERE cultura EQ @wg_header-cultura
     AND waerk   EQ @wg_header-waerk
     AND estado  EQ @wl_kna1-regio
     AND safra   EQ @wg_header-safra
     AND val_de  LE @wg_header-dtent
     AND val_ate GE @wg_header-dtent.
* #138092 -  ITSOUZA - 28.05.2024 15:59:54 - Fim

  IF wl_0040-status NE '' AND wl_0040-status NE 'R'.
    status_btn = abap_true.
  ELSE.

*#138092 -  ITSOUZA - 28.05.2024 15:59:54 - Inicio

    READ TABLE lt_zsdt0042 TRANSPORTING NO FIELDS WITH KEY witht = 'FR'.
    IF sy-subrc NE 0.
      status_btn = abap_true.
    ENDIF.
*    SELECT SINGLE *
*      FROM zsdt0043
*      INTO wl_0043
*       WHERE werks EQ wl_0040-vkbur
*         AND uname EQ sy-uname.
* #138092 -  ITSOUZA - 28.05.2024 15:59:54 - Fim

    IF wl_0043 IS INITIAL.
      status_btn = abap_true.
    ENDIF.

*    COUNT = REDUCE INT4( INIT Y = 0 FOR LS2 IN TG_ITENS WHERE ( VBELN IS NOT INITIAL ) NEXT Y = Y + 1 ).
*    IF COUNT IS NOT INITIAL.
*      STATUS_BTN = ABAP_TRUE.
*    ENDIF.
  ENDIF.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  IF tg_msg_ret[] IS NOT INITIAL.
    status_btn = abap_true.
  ENDIF.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  IF status_btn IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-name EQ 'BTN_FUN'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

*#138092 - ITSOUZA - Inicio


  READ TABLE lt_zsdt0042 TRANSPORTING NO FIELDS WITH KEY witht = 'FI'.
  IF sy-subrc EQ 0.
    IF wl_0043 IS NOT INITIAL.
      LOOP AT SCREEN.
        IF screen-name EQ 'BTN_FI'.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
  CLEAR lt_zsdt0042.
*#138092 - ITSOUZA - Fim
ENDMODULE.                 " STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.

  DATA: wl_tot_vlr TYPE zsdt0041-zwert.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  DATA: wl_texto_msg   TYPE char255.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  CASE sy-ucomm.
    WHEN 'CANCEL'
      OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'BTN_FUN'.
* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
      IF wg_0260-compra_fim_export = 'S'.
        "Simulador está vinculado a uma compra com Fins Exportação.
        MESSAGE TEXT-e83 TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 0.
      ELSE.
        IF ( wg_header-funrural IS NOT INITIAL ) AND ( wg_header-funuser IS INITIAL ).
          " O Cliente está cadastrado na Exceção para a Não retenção do FUNRURAL.
          " Deseja prosseguir com a Porcentagem de INSS para valor cheio?
          CONCATENATE TEXT-i04 TEXT-i05 INTO wl_texto_msg.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              text_question         = wl_texto_msg
              text_button_1         = 'Sim'
              text_button_2         = 'Não'
              start_column          = 30
              start_row             = 8
              display_cancel_button = ' '
            IMPORTING
              answer                = lv_resp.

          IF lv_resp NE 1.
            LEAVE SCREEN.
          ELSE.
            sy-ucomm = 'BTN_FUN'.
          ENDIF.
        ENDIF.
      ENDIF.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
      PERFORM atualiza_funrural.
      PERFORM limpa_variavel USING sy-ucomm.
      PERFORM busca_dados_doc.
      PERFORM busca_dados.
      PERFORM monta_memo.
      PERFORM calcula_header USING wl_tot_vlr.
      PERFORM calcula_itens.
      PERFORM grava_dados.

*  #138092 - ITSOUZA - Inicio
    WHEN 'BTN_FI'.

      wl_texto_msg = COND #( WHEN wg_header-fundeinfra_exce IS INITIAL THEN |{ TEXT-i08 } { TEXT-i09 }|
                             ELSE TEXT-i10 ).
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question         = wl_texto_msg
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          start_column          = 30
          start_row             = 8
          display_cancel_button = ' '
        IMPORTING
          answer                = lv_resp.

      IF lv_resp NE 1.
        LEAVE SCREEN.
      ELSE.
        sy-ucomm = 'BTN_FI'.
      ENDIF.
      btn_fi = abap_true.
      PERFORM atualiza_fundeinfra.
      PERFORM limpa_variavel USING sy-ucomm.
      PERFORM busca_dados_doc.
      PERFORM busca_dados.
      PERFORM monta_memo.
      PERFORM calcula_header USING wl_tot_vlr.
      PERFORM calcula_itens.
      PERFORM grava_dados.
      PERFORM verifica_erros.
*  #138092 - ITSOUZA - Fim
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  BUSCA_ANTECIPACAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_antecipacao INPUT.
  CLEAR: wg_header-antec.
  IF wg_header-tpsim+1(1) EQ 'P'
    OR wg_header-tpsim+1(1) EQ 'S'.
**        Taxa de juros
    SELECT SINGLE tx_juros
      FROM zsdt0039
      INTO wg_header-antec
       WHERE val_de         LE sy-datum
         AND val_ate        GE sy-datum.
*             AND FILIAL_ORIGEM  EQ WG_HEADER-VKBUR.

  ENDIF.
  IF wg_header-antec IS INITIAL.
    wg_header-antec = 12.
  ENDIF.
ENDMODULE.                 " BUSCA_ANTECIPACAO  INPUT
*&---------------------------------------------------------------------*
*&      Form  MONTA_MGLOBAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM monta_mglobal .
  DATA: BEGIN OF tl_itens_aux OCCURS 0.
          INCLUDE TYPE ty_itens.
  DATA:   matkl TYPE v023-wgbez,
        END OF tl_itens_aux,

        BEGIN OF tl_mara OCCURS 0,
          matnr TYPE mara-matnr,
          matkl TYPE mara-matkl,
        END OF tl_mara,

        BEGIN OF tl_v023 OCCURS 0,
          matkl TYPE t023t-matkl,
          wgbez TYPE t023t-wgbez,
        END OF tl_v023.

  DATA: wl_soma_mgcad TYPE zsdt0041-vlrtot,
        wl_soma_mgefe TYPE zsdt0041-vlrtot,
        wl_vlrtot     TYPE zsdt0041-vlrtot.

  REFRESH: tl_itens_aux, tl_mara, tl_v023, tg_mglobal.

  IF tg_itens[] IS NOT INITIAL.
    SELECT matnr matkl
      FROM mara
      INTO TABLE tl_mara
       FOR ALL ENTRIES IN tg_itens
        WHERE matnr EQ tg_itens-matnr.

    IF sy-subrc IS INITIAL.
      SELECT matkl wgbez
        FROM t023t
        INTO TABLE tl_v023
         FOR ALL ENTRIES IN tl_mara
         WHERE spras EQ sy-langu
           AND matkl EQ tl_mara-matkl.

    ENDIF.

  ENDIF.
  LOOP AT tg_itens.
    READ TABLE tl_mara
      WITH KEY matnr = tg_itens-matnr.
    IF sy-subrc IS INITIAL.
      READ TABLE tl_v023
        WITH KEY matkl = tl_mara-matkl.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING: tg_itens TO tl_itens_aux.
        MOVE: tl_v023-wgbez TO tl_itens_aux-matkl.

        APPEND tl_itens_aux.
      ENDIF.
    ENDIF.
    CLEAR: tl_itens_aux.
  ENDLOOP.

  SORT tl_v023 BY wgbez.
  DELETE ADJACENT DUPLICATES FROM tl_v023 COMPARING wgbez.

  SORT: tl_itens_aux BY matkl.
  CLEAR: wl_soma_mgcad, wl_soma_mgefe, wl_vlrtot.
  LOOP AT tl_v023.
    LOOP AT tl_itens_aux
      WHERE matkl EQ tl_v023-wgbez.

      wl_soma_mgcad = ( wl_soma_mgcad + ( tl_itens_aux-vlrtot * ( tl_itens_aux-mgcad / 100 ) ) ).
      wl_soma_mgefe = ( wl_soma_mgefe + ( tl_itens_aux-vlrtot * ( tl_itens_aux-mgefe / 100 ) ) ).
      ADD tl_itens_aux-vlrtot TO wl_vlrtot.

    ENDLOOP.
    tg_mglobal-mgcad = ( ( wl_soma_mgcad / wl_vlrtot ) * 100 ) .
    tg_mglobal-mgefe = ( ( wl_soma_mgefe / wl_vlrtot ) * 100 ) .
    tg_mglobal-matkl = tl_itens_aux-matkl.

    COLLECT tg_mglobal.
    CLEAR: tg_mglobal, wl_soma_mgcad, wl_soma_mgefe, wl_vlrtot.
  ENDLOOP.
ENDFORM.                    " MONTA_MGLOBAL
*&---------------------------------------------------------------------*
*&      Form  MOTA_LAYOUT_MGLOBAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mota_layout_mglobal .
  REFRESH: estrutura.
  PERFORM montar_estrutura_mglobal USING:
  1  'T023T'      'WGBEZ'    'TG_MGLOBAL' 'MATKL'    'Grp.Mercadoria'  ' ',
  2  'ZSDT0041'   'MGCAD'    'TG_MGLOBAL' 'MGCAD'    'Mrg.Cadastrada' ' ',
  3  'ZSDT0041'   'MGEFE'    'TG_MGLOBAL' 'MGEFE'    'Mrg.Efetiva'  ' '.
ENDFORM.                    " MOTA_LAYOUT_MGLOBAL
*&---------------------------------------------------------------------*
*&      Form  EXIBE_MGBLOBAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exibe_mgblobal .

  DATA: wl_layout TYPE  slis_layout_alv.
  wl_layout-zebra = c_x.
  wl_layout-window_titlebar = 'Simulador de Vendas - Insumos'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = v_report
*     I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
      is_layout             = wl_layout
      it_fieldcat           = estrutura[]
*     IT_SORT               = T_SORT[]
      i_save                = 'A'
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 51
      i_screen_end_line     = 13
    TABLES
      t_outtab              = tg_mglobal.
ENDFORM.                    " EXIBE_MGBLOBAL
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM montar_estrutura_mglobal USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen).

  DATA: x_contador TYPE string.
  CLEAR: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-outputlen     = x_contador.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  INPUTA_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM inputa_desc .
  DATA: BEGIN OF tl_descp OCCURS 0,
          field(50),
        END OF tl_descp,

        BEGIN OF tl_field OCCURS 0,
          tabname   TYPE dd03l-tabname,    "Nome da tabela
          fieldname TYPE dd03l-fieldname,    "Nome de campo
          s(1)      TYPE c,
        END OF tl_field,

        BEGIN OF tl_value OCCURS 0,
          tabname    TYPE dd03l-tabname,    "Nome da tabela
          fieldname  TYPE dd03l-fieldname,    "Nome de campo
          char79(79) TYPE c,
        END OF tl_value.

  DATA: tl_0046        TYPE TABLE OF zsdt0046 WITH HEADER LINE,
        tl_0047        TYPE TABLE OF zsdt0047 WITH HEADER LINE,
        tl_heading_tab TYPE  f4typ_heading_tab,
        wl_heading_tab LIKE LINE OF tl_heading_tab,
        wl_index       TYPE sy-tabix,
        wl_char(20).

  REFRESH: tl_0046, tl_0047, tl_value, tl_field, tl_descp, tl_heading_tab.

  IF tg_itens[] IS NOT INITIAL.
    READ TABLE tg_itens INDEX wg_selectedcell-row_id-index.
    SELECT *
      FROM zsdt0046
      INTO TABLE tl_0046
       FOR ALL ENTRIES IN tg_itens
       WHERE val_de  LE sy-datum
         AND val_ate GE sy-datum
         AND auart   EQ tg_itens-auart
         AND bukrs   EQ wa_t001k-bukrs.

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM zsdt0047
        INTO TABLE tl_0047
         FOR ALL ENTRIES IN tl_0046
          WHERE seq_desc EQ tl_0046-seq_desc
            AND usnam    EQ sy-uname.

      LOOP AT tl_0047.
        DELETE tl_0046 WHERE seq_desc EQ tl_0047-seq_desc.

      ENDLOOP.

      LOOP AT tl_0046.
        READ TABLE tl_descp TRANSPORTING NO FIELDS
          WITH KEY field = tl_0046-seq_desc.
        IF sy-subrc IS NOT INITIAL.
          MOVE: tl_0046-seq_desc TO tl_descp-field.
          APPEND tl_descp.
        ENDIF.

      ENDLOOP.

      tl_field-tabname = 'ZSDT0046'.
      tl_field-fieldname = 'SEQ_DESC'.
      tl_field-s = 'X'.
      APPEND  tl_field.

      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
        EXPORTING
          fieldname                 = 'DESCONTO'
          tabname                   = 'ZSDT0041'
        IMPORTING
          index                     = wl_index
          select_value              = wl_char
        TABLES
          fields                    = tl_field
          select_values             = tl_value
          valuetab                  = tl_descp
          heading_table             = tl_heading_tab
        EXCEPTIONS
          field_not_in_ddic         = 001
          more_then_one_selectfield = 002
          no_selectfield            = 003.

      IF sy-subrc IS INITIAL.
        READ TABLE tl_descp INDEX wl_index.
        LOOP AT tl_0046
           WHERE seq_desc EQ tl_descp-field.

          LOOP AT tg_itens
            WHERE auart EQ tl_0046-auart.

            MOVE tl_0046-vlr_desc TO tg_itens-desconto.

            MODIFY TABLE tg_itens.

          ENDLOOP.
          CLEAR: tg_itens, tl_0046.

        ENDLOOP.

        PERFORM calcula_itens.

        CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
          EXPORTING
            functioncode           = '/00'
          EXCEPTIONS
            function_not_supported = 1.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " INPUTA_DESC
*&---------------------------------------------------------------------*
*&      Form  GERA_ORDEM_VENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_ordem_venda.
  DATA: wl_vbeln    TYPE vbak-vbeln,
        wl_erro(1),
        at_contador TYPE int4.

  CALL FUNCTION 'ZSDMF001_GERA_OV_SIMULADOR'
    EXPORTING
      i_doc_simulacao = wg_header-doc_simulacao
    IMPORTING
      erro            = wl_erro.

  CHECK wl_erro IS INITIAL.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = c_atual
    EXCEPTIONS
      function_not_supported = 1.

  at_contador = 0.

  DO.
    IF at_contador EQ 10.
      EXIT.
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = |Processando 40 { at_contador }|.

    PERFORM reajuste_41.
    ADD 1 TO at_contador.

  ENDDO.

*-CS2019001753-05.01.2023-#65741-JT-inicio
  PERFORM f_cria_tabelas_ov(saplzgfsd001) USING wg_header-doc_simulacao.
*-CS2019001753-05.01.2023-#65741-JT-fim

ENDFORM.                    " GERA_ORDEM_VENDA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_matnr .

  IF wg_header-tpsim IS NOT INITIAL.
    IF NOT wg_header-vkbur IS INITIAL.
      PERFORM organiza_matnr.
      PERFORM exibe_matnr.
    ELSE.
      MESSAGE 'Escritório de Venda não Informado!' TYPE 'S'.
    ENDIF.
  ENDIF.
ENDFORM.                    " DISPLAY_MATNR
*&---------------------------------------------------------------------*
*&      Form  MOTA_LAYOUT_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM monta_layout_matnr .
  REFRESH: estrutura.
  PERFORM montar_estrutura_matnr USING:
     1  'MARA'       'MATNR'           'TG_MATNR'   'MATNR'           ' '  ' ',
     2  'MAKT'       'MAKTX'           'TG_MATNR'   'MAKTX'           ' '  ' ',
     3  'ZSDT0036'   'MEINS'           'TG_MATNR'   'MEINS'           ' '  ' ',
     4  'ZSDT0036'   'WERKS_FORNEC'    'TG_MATNR'   'WERKS_FORNEC'    ' '  ' ',
     5  'ZSDT0036'   'INCO1'           'TG_MATNR'   'INCO1'           ' '  ' '.
ENDFORM.                    " MONTA_LAYOUT_MATNR
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM montar_estrutura_matnr USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen).

  DATA: x_contador TYPE string.
  CLEAR: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-outputlen     = x_contador.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  EXIBE_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exibe_matnr.
  DATA: wl_layout TYPE  slis_layout_alv.
  PERFORM definir_eventos.
  PERFORM monta_layout_matnr.

  v_report = sy-repid.

  wl_layout-zebra = c_x.
  wl_layout-box_fieldname = 'MARK'.
  wl_layout-box_tabname  = 'TG_MATNR'.
  wl_layout-window_titlebar = 'Simulador de Vendas - Material'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = v_report
      i_callback_user_command = 'XUSER_COMMAND'
      is_layout               = wl_layout
      it_fieldcat             = estrutura[]
*     IT_SORT                 = T_SORT[]
      i_save                  = 'A'
      i_screen_start_column   = 3
      i_screen_start_line     = 3
      i_screen_end_column     = 95
      i_screen_end_line       = 13
    TABLES
      t_outtab                = tg_matnr.

ENDFORM.                    " EXIBE_MATNR
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM organiza_matnr .
  TYPES: BEGIN OF tyl_mara,
           matnr TYPE mara-matnr,
           matkl TYPE setleaf-valfrom,
           mtart TYPE mtart,
         END OF tyl_mara.

  DATA: BEGIN OF wl_0036,
          val_de       TYPE zsdt0036-val_de,
          val_ate      TYPE zsdt0036-val_ate,
          dtvenc       TYPE zsdt0036-dtvenc,
          matnr        TYPE zsdt0036-matnr,
          waerk        TYPE zsdt0036-waerk,
          inco1        TYPE zsdt0036-inco1,
          safra        TYPE zsdt0036-safra,
          cultura      TYPE zsdt0036-cultura,
          meins        TYPE zsdt0036-meins,
          werks_fornec TYPE zsdt0036-werks_fornec,
          vlr_custo    TYPE zsdt0036-vlr_custo,
          perc_margem  TYPE zsdt0036-perc_margem,
          vlr_margem   TYPE zsdt0036-vlr_margem,
          vlr_venda    TYPE zsdt0036-vlr_venda,
          loekz        TYPE zsdt0036-loekz,
          bukrs        TYPE zsdt0036-bukrs,
        END OF wl_0036.

  DATA: tl_0036     LIKE TABLE OF wl_0036,
        tl_makt     TYPE TABLE OF makt,
        wl_makt     TYPE makt,
        tl_setleaf  TYPE TABLE OF setleaf,
        wl_setleaf  TYPE setleaf,
        tl_setlinet TYPE TABLE OF setlinet,
        wl_setlinet TYPE setlinet,
        tl_mara     TYPE TABLE OF tyl_mara,
        wl_mara     TYPE tyl_mara.

  DATA: rgl_matnr  TYPE RANGE OF mara-matnr,
        wr_matnr   LIKE LINE OF rgl_matnr,
        rg_cultura TYPE RANGE OF zsdt0036-cultura,
        wr_cultura LIKE LINE OF rg_cultura.

  REFRESH: rgl_matnr, tl_makt, tl_0036, tl_mara, tl_setlinet, tl_setleaf, rg_cultura.
  CLEAR: rgl_matnr, wr_matnr, wl_makt, wl_0036,
         wl_setleaf, wl_setlinet, wl_mara.

  IF wg_header-cultura IS NOT INITIAL.
    wr_cultura-sign    = 'I'.
    wr_cultura-option  = 'EQ'.
    wr_cultura-low     = wg_header-cultura.

    APPEND wr_cultura TO rg_cultura.
    CLEAR: wr_cultura.
  ENDIF.
  wr_cultura-sign    = 'I'.
  wr_cultura-option  = 'EQ'.
  wr_cultura-low     =  space.

  APPEND wr_cultura TO rg_cultura.
  CLEAR: wr_cultura.


  SELECT val_de val_ate dtvenc matnr waerk inco1 safra cultura meins
         werks_fornec vlr_custo perc_margem vlr_margem vlr_venda loekz bukrs
    FROM zsdt0036
    INTO TABLE tl_0036
      WHERE loekz     EQ space
        AND waerk     EQ wg_header-waerk
        AND cultura   IN rg_cultura
        AND safra     EQ wg_header-safra
        AND eliminado EQ space
        AND bukrs     EQ wa_t001k-bukrs.


  LOOP AT tl_0036 INTO wl_0036.
    IF wl_0036-cultura IS NOT INITIAL
    AND wl_0036-cultura NE wg_header-cultura.
      DELETE TABLE tl_0036 FROM wl_0036.
    ENDIF.

    IF  wl_0036-val_de  LE sy-datum
   AND  wl_0036-val_ate GE sy-datum.
    ELSE.
      DELETE TABLE tl_0036 FROM wl_0036.
    ENDIF.
  ENDLOOP.

  IF tl_0036[] IS NOT INITIAL.
    SELECT *
      FROM makt
      INTO TABLE tl_makt
       FOR ALL ENTRIES IN tl_0036
        WHERE matnr EQ tl_0036-matnr
          AND spras EQ sy-langu.

    SELECT matnr matkl mtart
      FROM mara
      INTO TABLE tl_mara
       FOR ALL ENTRIES IN tl_0036
       WHERE matnr EQ tl_0036-matnr.

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM setleaf
        INTO TABLE tl_setleaf
         FOR ALL ENTRIES IN tl_mara
         WHERE setname EQ 'MAGGI_ZSDT0044_02'
          AND valfrom  EQ tl_mara-matkl.

      IF sy-subrc IS INITIAL.
        SELECT *
          FROM setlinet
          INTO TABLE tl_setlinet
          FOR ALL ENTRIES IN tl_setleaf
           WHERE setname EQ 'MAGGI_ZSDT0044_02'
             AND langu   EQ sy-langu
             AND lineid  EQ tl_setleaf-lineid.
      ENDIF.

    ENDIF.

  ENDIF.

  REFRESH tg_matnr.
  CLEAR tg_matnr.
  SORT: tl_makt     BY matnr,
        tl_mara     BY matnr,
        tl_setleaf  BY valfrom,
        tl_setlinet BY lineid.

  LOOP AT tl_0036 INTO wl_0036.
    READ TABLE tl_makt INTO wl_makt
      WITH KEY matnr = wl_0036-matnr
               BINARY SEARCH.

    READ TABLE tl_mara INTO wl_mara
      WITH KEY matnr = wl_0036-matnr
               BINARY SEARCH.

    READ TABLE tl_setleaf INTO wl_setleaf
          WITH KEY valfrom = wl_mara-matkl.
    IF sy-subrc IS INITIAL.
      READ TABLE tl_setlinet INTO wl_setlinet
        WITH KEY lineid = wl_setleaf-lineid.

      MOVE: wl_setlinet-descript TO tg_matnr-auart.
    ELSE.
      CLEAR: tg_matnr-auart.
    ENDIF.

    IF tg_matnr-auart IS NOT INITIAL.
      READ TABLE tg_setleaf INTO tg_setleaf
        WITH KEY valfrom = tg_matnr-auart.

      IF sy-subrc IS INITIAL.
        READ TABLE tg_setlinet INTO tg_setlinet
          WITH KEY lineid = tg_setleaf-lineid.

        MOVE: tg_setlinet-descript TO tg_matnr-spart.
      ENDIF.
*      comentado o ZDEF conforme chamado CS2016000364
      IF  tg_matnr-auart EQ 'ZFTE'.
*        TG_MATNR-AUART EQ 'ZDEF' OR

        MOVE: wg_header-safra TO tg_matnr-charg.
      ENDIF.
    ELSE.
      CLEAR: tg_matnr-spart.
    ENDIF.

**** Clear no campo Charg CS2016001142>>>>>
    CASE tg_matnr-auart.
      WHEN 'ZFTE' OR 'ZOFE'.
*        IF wl_0036-werks_fornec EQ '0175' AND wl_mara-mtart EQ 'ZFER'.
        IF wl_0036-werks_fornec IN r_werks AND wl_mara-mtart EQ 'ZFER'.
          CLEAR: tg_matnr-charg.
        ENDIF.
    ENDCASE.
**** Clear no campo Charg CS2016001142<<<<<


    MOVE-CORRESPONDING: wl_0036 TO tg_matnr.
    MOVE:  wl_makt-maktx        TO tg_matnr-maktx,
           wl_0036-werks_fornec TO tg_matnr-werks_fornec,
           wl_0036-inco1        TO tg_matnr-inco1,
           wl_0036-dtvenc       TO tg_matnr-dtvenc.

    APPEND tg_matnr.
    CLEAR: tg_matnr.
  ENDLOOP.

  SORT tg_matnr BY auart.

ENDFORM.                    " ORGANIZA_MATNR
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM definir_eventos .

  PERFORM f_carregar_eventos USING:
                                   slis_ev_user_command 'XUSER_COMMAND'.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_4780   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.

ENDFORM.                    " F_CARREGAR_EVENTOS
*---------------------------------------------------------------------*
*       FORM XUSER_COMMAND                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xuser_command USING ucomm    LIKE sy-ucomm
                         selfield TYPE kkblo_selfield..     "#EC CALLED

  DATA: tl_itens_aux LIKE TABLE OF tg_itens,
        wl_itens     LIKE LINE OF tg_itens,
        wl_lines     TYPE sy-tabix.
  REFRESH: tl_itens_aux.

  CASE sy-ucomm.

    WHEN '&ONT'.
      IF wg_header-tpsim IS NOT INITIAL.
        tl_itens_aux[] = tg_itens[].
        REFRESH: tg_itens.
        LOOP AT tl_itens_aux INTO wl_itens.
          wl_itens-posnr = sy-tabix * 10.
          APPEND wl_itens TO tg_itens.
        ENDLOOP.
        CLEAR: wl_itens, wl_lines.
        DESCRIBE TABLE tg_itens LINES wl_lines.

        LOOP AT tg_matnr WHERE mark IS NOT INITIAL.

          SELECT SINGLE vlr_frete
              FROM zsdt0037
              INTO (wl_itens-vlr_frete)
                 WHERE val_de          LE sy-datum
                   AND val_ate         GE sy-datum
                   AND meins           EQ tg_matnr-meins
                   AND filial_origem   EQ tg_matnr-werks_fornec
                   AND filial_destino  EQ wg_header-vkbur
                   AND matkl           EQ ( SELECT matkl FROM mara WHERE matnr EQ tg_matnr-matnr )
                   AND waers           EQ 'BRL'
                   AND bukrs           EQ wa_t001k-bukrs.

          CASE tg_matnr-inco1.
            WHEN 'CIF' OR 'CPT' OR 'CFR'.
*              PERFORM ATUALIZA_FRETE CHANGING WL_ITENS-VLR_FRETE.
            WHEN OTHERS.
              wl_itens-vlr_frete = 0.
          ENDCASE.

          ADD 1 TO wl_lines.
          wl_itens-posnr = wl_lines * 10 .
          wl_itens-matnr  = tg_matnr-matnr.
          wl_itens-maktx  = tg_matnr-maktx.
          wl_itens-zieme  = tg_matnr-meins.
          wl_itens-werks  = tg_matnr-werks_fornec.
          wl_itens-inco1  = tg_matnr-inco1.
          wl_itens-auart  = tg_matnr-auart.
          wl_itens-charg  = tg_matnr-charg.
          wl_itens-spart  = tg_matnr-spart.
          wl_itens-dtvenc = tg_matnr-dtvenc.

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
          IF wl_itens-cultura_apl IS INITIAL.
            IF wg_cultura_apl IS NOT INITIAL.
              wl_itens-cultura_apl = wg_cultura_apl.
            ENDIF.
          ENDIF.

          IF wl_itens-safra_apl IS INITIAL.
            wl_itens-safra_apl = wg_header-safra.
          ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

          APPEND wl_itens TO tg_itens.
        ENDLOOP.

        PERFORM calcula_itens.

      ENDIF.
      CALL METHOD grid1->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
  ENDCASE.
ENDFORM. "XUSER_COMMAND
*&---------------------------------------------------------------------*
*&      Module  GET_CURSOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_cursor INPUT.
  DATA: l_cursor TYPE t_cursor.

  GET CURSOR FIELD l_cursor-fname LINE l_cursor-pos AREA l_cursor-tc
             VALUE l_cursor-value.

  IF l_cursor-fname EQ  'WG_HEADER-WAERK'
  OR l_cursor-fname EQ  'WG_HEADER-SAFRA'
  OR l_cursor-fname EQ  'WG_HEADER-VKBUR'
  OR l_cursor-fname EQ  'WG_HEADER-CULTURA'.
    PERFORM calcula_itens.
  ENDIF.

ENDMODULE.                 " GET_CURSOR  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_SHDB_FAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WL_VBELN  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
FORM f_shdb_fat  TABLES it_msg
                 CHANGING p_vbeln
                          p_erro
                          p_doc.

  REFRESH ti_bdcdata.
  PERFORM f_bdc_data USING:
     'SAPMV60A'  '0102'  'X'  ''                 ' ',
     ''          ''      ''   'BDC_OKCODE'        '/00',
     ''          ''      ''   'KOMFK-VBELN(01)'    p_vbeln,

     'SAPMV60A'  '0104'  'X'  ''                 ' ',
     ''          ''      ''   'BDC_OKCODE'        '=SICH'.


  CLEAR p_erro.
  PERFORM zf_call_transaction TABLES it_msg USING 'VF01' CHANGING p_erro p_doc.
ENDFORM.                    " F_SHDB_FAT

*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PROGRAM  text
*      -->P_DYNPRO   text
*      -->P_START    text
*      -->P_FNAM     text
*      -->P_FVAL     text
*----------------------------------------------------------------------*
FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.

  CLEAR wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  APPEND wa_bdcdata TO ti_bdcdata.

ENDFORM.                    " F_BDC_DATA

*&---------------------------------------------------------------------*
*&      Form  BATCH_INPUT
*&---------------------------------------------------------------------*
FORM batch_input  USING     VALUE(p_flag)
                            VALUE(p_fnam)
                            VALUE(p_fval).

  CLEAR wa_bdcdata.

  IF NOT p_flag IS INITIAL.
    wa_bdcdata-program  = p_fnam.
    wa_bdcdata-dynpro   = p_fval.
    wa_bdcdata-dynbegin = 'X'.
  ELSE.
    wa_bdcdata-fnam = p_fnam.
    wa_bdcdata-fval = p_fval.
  ENDIF.

  APPEND wa_bdcdata TO ti_bdcdata.

ENDFORM.                    "batch_input

*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRANS    text
*      -->P_ERRO     text
*----------------------------------------------------------------------*
FORM zf_call_transaction TABLES it_msg_ret USING p_trans CHANGING p_erro p_doc.

  DATA: c_msgid LIKE it_msg-msgid VALUE 'VF',
        c_msgnr LIKE it_msg-msgnr VALUE '311'.

  REFRESH it_msg.

  IF p_trans EQ 'FB08'.
    c_msgid = 'F5'.
    c_msgnr = '312'.
  ENDIF.

  wl_mode = 'E'.
  CALL TRANSACTION p_trans USING ti_bdcdata
        MODE wl_mode
        MESSAGES INTO it_msg.

  READ TABLE it_msg WITH KEY msgtyp = 'A'.
  IF sy-subrc = 0.
    p_erro = 'X'.
  ELSE.
    READ TABLE it_msg WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      p_erro = 'X'.
    ENDIF.
  ENDIF.

  CLEAR wg_documento.

  READ TABLE it_msg WITH KEY msgid = c_msgid
                             msgnr = c_msgnr
                             msgtyp = 'S'.

  IF sy-subrc = 0.
    MOVE it_msg-msgv1 TO wg_documento.
  ENDIF.

  IF  wg_documento IS INITIAL.
    p_erro = 'X'.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_documento
      IMPORTING
        output = wg_documento.
  ENDIF.
  p_doc = wg_documento.
  it_msg_ret[] = it_msg[].

ENDFORM.                    "ZF_CALL_TRANSACTION

*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_INFOR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos_infor OUTPUT.
  DATA: tl_tspat TYPE TABLE OF tspat WITH HEADER LINE.

**  Seleciona dados a serem exibidosno ALV com base no ALV principal
  SELECT *
    FROM tspat
    INTO TABLE tl_tspat
    FOR ALL ENTRIES IN tg_itens
    WHERE spart EQ tg_itens-spart
     AND  spras EQ sy-langu.

  FREE tg_infor.
  CLEAR tg_infor.

  LOOP AT tg_itens.
    READ TABLE tg_infor WITH KEY spart = tg_itens-spart.
    IF sy-subrc IS INITIAL.
      ADD tg_itens-vlrtot TO tg_infor-vlrtot.
      MODIFY tg_infor TRANSPORTING vlrtot WHERE spart = tg_itens-spart.
    ELSE.
      MOVE-CORRESPONDING tg_itens TO tg_infor.
      READ TABLE tl_tspat WITH KEY spart = tg_itens-spart.
      tg_infor-vtext = tl_tspat-vtext.
      APPEND tg_infor.
    ENDIF.
  ENDLOOP.

  IF obj_cont_infor IS INITIAL.

    wa_layout-zebra      = abap_true.
    wa_layout-no_rowmark = abap_true.
*    WA_STABLE-ROW        = ABAP_TRUE.

    CREATE OBJECT obj_cont_infor
      EXPORTING
        container_name = 'OBJ_INFOR'.

    CREATE OBJECT obj_grid_infor
      EXPORTING
        i_parent = obj_cont_infor.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    PERFORM montar_layout_infor.

    CALL METHOD obj_grid_infor->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_fieldcatalog      = t_fieldcat_infor[]
        it_outtab            = tg_infor[].

  ELSE.
    CALL METHOD obj_grid_infor->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS_INFOR  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_ESC_VEDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->C_ERRO     text
*----------------------------------------------------------------------*
FORM f_validar_esc_venda USING    p_vkbur TYPE zsdt0040-vkbur
                        CHANGING c_erro  TYPE sy-subrc.
  DATA: tl_0060 TYPE TABLE OF zsdt0060 WITH HEADER LINE,
        lv_erro TYPE i,
        lv_msg  TYPE c LENGTH 255.

  CLEAR c_erro.

  SELECT *
    INTO TABLE tl_0060
    FROM zsdt0060
    WHERE usnam EQ sy-uname
     AND  programa EQ sy-cprog.

  READ TABLE tl_0060 WITH KEY vkbur = p_vkbur.
  IF sy-subrc IS NOT INITIAL.
    CONCATENATE 'Sem permissão para visualizar vendas do escritório ' p_vkbur '.' INTO lv_msg SEPARATED BY space.
    MESSAGE lv_msg TYPE 'E' DISPLAY LIKE 'I'.
    sy-subrc = lv_erro.

  ENDIF.

ENDFORM.                    "F_VALIDA_ESC_VEDA
*&---------------------------------------------------------------------*
*&      Form  IMPRIMI_CONTRATO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WG_HEADER_DOC_SIMULACAO  text
*----------------------------------------------------------------------*
FORM imprimi_contrato.

*  SELECT SINGLE * FROM ZSDT0040 INTO WA_0041
*    WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.
*
*  IF WA_0041 IS NOT INITIAL.
*
*    MOVE WA_0041-DT_ENTREGA_SEM TO WA_PRINT-DT_ENTREGA_SEM.
*    MOVE WA_0041-DT_ENTREGA_DEF TO WA_PRINT-DT_ENTREGA_DEF.
*    MOVE WA_0041-DT_ENTREGA_FET TO WA_PRINT-DT_ENTREGA_FET.
*    MOVE WA_0041-JUROS_MORA     TO WA_PRINT-JUROS_MORA.
*    MOVE WA_0041-FIADOR_01      TO WA_PRINT-FIADOR_01.
*    MOVE WA_0041-FIADOR_02      TO WA_PRINT-FIADOR_02.
*
*    IF WA_0041-PAG_PRORROGADO IS NOT INITIAL.
*      MOVE WA_0041-PAG_PRORROGADO TO WA_PRINT-CHECKYES.
*    ELSE.
*      MOVE 'X' TO WA_PRINT-CHECKYES.
*    ENDIF.
*
*  ENDIF.

*  CALL FUNCTION 'K_KKB_POPUP_RADIO2'
*    EXPORTING
*      I_TITLE   = |Imprimir Ordens da S.V?|
*      I_TEXT1   = |SIM!|
*      I_TEXT2   = |NÃO!|
*      I_DEFAULT = '1'
*    IMPORTING
*      I_RESULT  = W_ANSWER
*    EXCEPTIONS
*      CANCEL    = 1
*      OTHERS    = 2.



  SELECT COUNT(*)
    FROM zsdt0090
    WHERE doc_simulacao EQ wg_header-doc_simulacao
    AND flag NE abap_true
    AND categoria EQ 'O'
    AND estorno EQ abap_false.

  IF sy-subrc IS INITIAL.
    MESSAGE |Agregar Desconto para Emitir o Contrato! | TYPE 'I' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*--------------------------------------------
* este bloco esta sendo implementado pelo JAime - Projeto Insumos
* Gerador de Documentos
*--------------------------------------------
*-CS2019001753-04.01.2023-#65741-JT-inicio
  SELECT SINGLE nr_venda
    FROM zsdt0310
   WHERE nr_venda      = @wg_header-doc_simulacao
     AND tipo_doc      = 'CTR'
     AND id_documento IS NOT INITIAL
     AND status       <> '10'
    INTO @DATA(l_nr_venda). "Cancelado

  IF sy-subrc = 0.
    CALL FUNCTION 'ZSD_INSUMOS_COCKPIT'
      EXPORTING
        i_popup         = abap_true
        i_sintet        = abap_false
        i_analit        = abap_true
        i_doc_simulacao = wg_header-doc_simulacao
        i_doctod        = abap_true
        i_todos         = abap_true.
  ELSE.
*-CS2019001753-04.01.2023-#65741-JT-fim
    CALL SCREEN 300 STARTING AT 35 05.
  ENDIF.
******  CALL SCREEN 210 ENDING AT 68 15 STARTING AT 3 3.

ENDFORM.                    " IMPRIMI_CONTRATO
*&---------------------------------------------------------------------*
*&      Module  STATUS_0210  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0210 OUTPUT.
  SET PF-STATUS 'S0210'.
  SET TITLEBAR 'T0210'.
  DATA: cont TYPE sy-subrc.

  CLEAR: cont.
  READ TABLE tg_itens TRANSPORTING NO FIELDS WITH KEY auart = 'ZSEM'.  cont = cont + sy-subrc.
  READ TABLE tg_itens TRANSPORTING NO FIELDS WITH KEY auart = 'ZOSM'.  cont = cont + sy-subrc.
  IF cont > 4.
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'ZSDT0040-DT_ENTREGA_SEM'.
          screen-active = 0.
          MODIFY SCREEN.
        WHEN 'WA_PRINT-DT_ENTREGA_SEM'.
          screen-active = 0.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.

  ENDIF.

  CLEAR: cont.
  READ TABLE tg_itens TRANSPORTING NO FIELDS WITH KEY auart = 'ZFTE'.  cont = cont + sy-subrc.
  READ TABLE tg_itens TRANSPORTING NO FIELDS WITH KEY auart = 'ZOFE'.  cont = cont + sy-subrc.
  IF cont > 4.
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'ZSDT0040-DT_ENTREGA_FET'.
          screen-active = 0.
          MODIFY SCREEN.
        WHEN 'WA_PRINT-DT_ENTREGA_FET'.
          screen-active = 0.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.

  ENDIF.

  CLEAR: cont.
  READ TABLE tg_itens TRANSPORTING NO FIELDS WITH KEY auart = 'ZDEF'.  cont = cont + sy-subrc.
  READ TABLE tg_itens TRANSPORTING NO FIELDS WITH KEY auart = 'ZODF'.  cont = cont + sy-subrc.
  IF cont > 4.
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'ZSDT0040-DT_ENTREGA_DEF'.
          screen-active = 0.
          MODIFY SCREEN.
        WHEN 'WA_PRINT-DT_ENTREGA_DEF'.
          screen-active = 0.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.

  ENDIF.

  IF wa_print-fiador_01 IS NOT INITIAL.
    SELECT SINGLE name1
      FROM kna1
      INTO (fiador_01)
      WHERE kunnr EQ wa_print-fiador_01.
  ENDIF.

  IF wa_print-fiador_02 IS NOT INITIAL.
    SELECT SINGLE name1
    FROM kna1
    INTO (fiador_02)
    WHERE kunnr EQ wa_print-fiador_02.
  ENDIF.

ENDMODULE.                 " STATUS_0210  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0210 INPUT.
  CASE sy-ucomm.
    WHEN 'SAVEC'.
      PERFORM add_campos USING wg_header-doc_simulacao.
    WHEN 'CANCELC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0210  INPUT

*&---------------------------------------------------------------------*
*&      Form  ADD_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_campos USING doc_simulacao.

  DATA: vl_formname TYPE tdsfname,
        vl_name     TYPE rs38l_fnam,
        i_vbeln     TYPE vbeln.

  wa_0041-dt_entrega_sem = wa_print-dt_entrega_sem.
  wa_0041-dt_entrega_def = wa_print-dt_entrega_def.
  wa_0041-dt_entrega_fet = wa_print-dt_entrega_fet.
  wa_0041-juros_mora     = wa_print-juros_mora.
  wa_0041-fiador_01      = wa_print-fiador_01.
  wa_0041-fiador_02      = wa_print-fiador_02.


  IF sim EQ 'X'.
    wa_0041-pag_prorrogado =  sim.
  ELSEIF nao EQ 'X'.
    wa_0041-pag_prorrogado =  sim.
  ENDIF.

  IF doc_simulacao IS NOT INITIAL.
*    UPDATE ZSDT0040 SET DT_ENTREGA_SEM = WA_0041-DT_ENTREGA_SEM
*                        DT_ENTREGA_DEF = WA_0041-DT_ENTREGA_DEF
*                        DT_ENTREGA_FET = WA_0041-DT_ENTREGA_FET
*                        JUROS_MORA     = WA_0041-JUROS_MORA
*                        FIADOR_01      = WA_0041-FIADOR_01
*                        FIADOR_02      = WA_0041-FIADOR_02
*                        PAG_PRORROGADO = WA_0041-PAG_PRORROGADO
*                        WHERE DOC_SIMULACAO EQ DOC_SIMULACAO.
*
*    COMMIT WORK.
*    WAIT UP TO 2 SECONDS.

    PERFORM imprime_contrato USING doc_simulacao.

  ENDIF.

  LEAVE TO SCREEN 0.

ENDFORM.                    " ADD_CAMPOS
*&-----------------------------------------------------.----------------*
*&      Form  IMPRIME_CONTRATO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_contrato USING i_doc.

  CALL FUNCTION 'Z_GERA_CONTRATO'
    EXPORTING
      i_doc = i_doc
      i_dir = w_answer.

ENDFORM.                    " UPPER_LOWER
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_ORDEM_VENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_ordem_venda .
  DATA tg_itens_41 TYPE TABLE OF zsdt0041.

  FIELD-SYMBOLS <w0041> TYPE zsdt0041.

  CLEAR tg_itens_41.

  SORT it_itens_id BY index.
  DELETE ADJACENT DUPLICATES FROM it_itens_id COMPARING index.

  CHECK NOT it_itens_id IS INITIAL.

  SELECT *
    FROM zsdt0041
    INTO TABLE tg_itens_41
 FOR ALL ENTRIES IN it_itens_id
   WHERE doc_simulacao EQ it_itens_id-doc
     AND posnr         EQ it_itens_id-posnr
     AND vbeln         EQ it_itens_id-vbeln.

  IF ( tg_itens_41 IS NOT INITIAL ).
    CALL FUNCTION 'ZSDMF001_ATUALI_OV_SIMULADOR'
      TABLES
        ti_itens_ov       = tg_itens_41
      EXCEPTIONS
        ov_nao_encontrada = 1
        OTHERS            = 2.

    LOOP AT tg_itens_41 ASSIGNING <w0041>.
      <w0041>-desc_absoluto = it_itens_id[ doc = <w0041>-doc_simulacao
                                         posnr = <w0041>-posnr
                                         vbeln = <w0041>-vbeln ]-vlr_dif.
      <w0041>-desc_absoluto = <w0041>-desc_absoluto * -1.
    ENDLOOP.

*    DISPARO DO HEDGE PARA DESCONTO ABSOLUTO "VENDA"
    IF wg_header-waerk EQ 'BRL'.
      CASE wg_header-tpsim.
        WHEN 'BN' OR 'PM'.
        WHEN OTHERS.
          zcl_webservice_tx_curva=>hedge_insumos( i_numero = wg_header-doc_simulacao
                                                  i_acao   = 'DSC_ABS'
                                                  i_tipo   = 'VDI'
                                                  i_itens = tg_itens_41[]
                                                 ).
      ENDCASE.
    ENDIF.

*    DISPARO DO HEDGE PARA DESCONTO ABSOLUTO "FRETE"
    IF wg_header-tpsim NE 'PM'.
      zcl_webservice_tx_curva=>hedge_insumos( i_numero = wg_header-doc_simulacao
                                              i_acao   = 'DSC_ABS'
                                              i_tipo   = 'FRI'
                                              i_itens = tg_itens_41[]
                                             ).
    ENDIF.
  ENDIF.

  FREE it_itens_id.
ENDFORM.                    " ATUALIZA_ORDEM_VENDA
*&---------------------------------------------------------------------*
*&      Form  F_VAL_SACAS_USER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM f_val_sacas_user  CHANGING c_erro.

  CLEAR c_erro.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_ZSDT0044_06'
    TABLES
      set_values    = t_usermd
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  SORT t_usermd BY from.
  READ TABLE t_usermd WITH KEY from = sy-uname.

  c_erro = sy-subrc.


ENDFORM.                    " F_VAL_SACAS_USER

*&---------------------------------------------------------------------*
*&      Form  f_format-value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VALOR    STRING
*----------------------------------------------------------------------*
FORM f_format_value CHANGING p_valor.

  DATA:
    obj_js    TYPE REF TO cl_java_script,
    my_script TYPE string.

  CONCATENATE 'function numeroParaMoeda(n, c, d, t)'
              '{'
              'c = isNaN(c = Math.abs(c)) ? 2 : c,'
              'd = d == undefined ? "," : d,'
              't = t == undefined ? "." : t,'
              's = n < 0 ? "-" : "",'
              'i = parseInt(n = Math.abs(+n || 0).toFixed(c)) + "",'
              'j = (j = i.length) > 3 ? j % 3 : 0;'
              'return s + (j ? i.substr(0, j) + t : "") +'
              'i.substr(j).replace(/(\d{3})(?=\d)/g, "$1" + t) +'
              '(c ? d + Math.abs(n - i).toFixed(c).slice(2) : "");'
              '}'
              'var vlr = numeroParaMoeda (' p_valor ')'
              INTO my_script SEPARATED BY cl_abap_char_utilities=>cr_lf.

  obj_js = cl_java_script=>create( ).

  obj_js->compile( EXPORTING script_name = 'script.js'
                             script      = my_script ).

  obj_js->execute( EXPORTING script_name = 'script.js' ).
  p_valor = obj_js->get( name = 'vlr').

ENDFORM.                    "f_format-value
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen_0103 OUTPUT.

  DATA: values       TYPE vrm_values WITH HEADER LINE.
  FIELD-SYMBOLS: <fs_campo> TYPE any.
  DATA: it_vbfa TYPE TABLE OF vbfa,
        wa_vbfa TYPE vbfa,
        it_vbap TYPE TABLE OF vbap,
        wa_vbap TYPE vbap,
        it_vbak TYPE TABLE OF vbak,
        wa_vbak TYPE vbak,
        it_41   TYPE TABLE OF zsdt0041,
        it_90   TYPE TABLE OF zsdt0090 WITH HEADER LINE,
        wa_41   TYPE zsdt0041.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  DATA: values_cpg_ant TYPE vrm_values WITH HEADER LINE,
        values_cpg     TYPE vrm_values WITH HEADER LINE.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8


  PERFORM valida_layout TABLES t_fieldcatalog
                        USING sy-uname.


  IF init IS INITIAL.
    SELECT *
      FROM setleaf
      INTO TABLE tg_setleaf_cult
       WHERE setname EQ 'MAGGI_ZSDT0044_03'.

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM setlinet
        INTO TABLE tg_setlinet_cult
        FOR ALL ENTRIES IN tg_setleaf_cult
         WHERE setname EQ tg_setleaf_cult-setname
           AND langu   EQ sy-langu
           AND lineid  EQ tg_setleaf_cult-lineid.
    ENDIF.
    LOOP AT tg_setleaf_cult.
      READ TABLE tg_setlinet_cult
        WITH KEY setname = tg_setleaf_cult-setname
                 lineid  = tg_setleaf_cult-lineid.
      IF sy-subrc IS INITIAL.

        values-text = tg_setlinet_cult-descript.
        values-key  = tg_setleaf_cult-valfrom.
        APPEND values.
      ENDIF.
    ENDLOOP.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = 'WG_HEADER-TPCULT'
        values          = values[]
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    REFRESH: values.
    CLEAR: values.

    values-text = 'CIF'.
    values-key  = 'CIF'.
    APPEND values.

    values-text = 'FOB'.
    values-key  = 'FOB'.
    APPEND values.

    values-text = 'CPT'.
    values-key  = 'CPT'.
    APPEND values.

    values-text = 'CFR'.
    values-key  = 'CFR'.
    APPEND values.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = 'WG_HEADER-FCULT'
        values          = values[]
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.


    ENDIF.

    REFRESH values.
    values-text = 'Adiantamento'.
    values-key  = 'AD'.
    APPEND values.

    values-text = 'Venda A Vista'.
    values-key  = 'VV'.
    APPEND values.

    values-text = 'Venda A Prazo'.
    values-key  = 'VP'.
    APPEND values.

    values-text = 'Troca Safra'.
    values-key  = 'TS'.
    APPEND values.

      values-text = 'Troca a Vista'.
      values-key  = 'TV'.
      APPEND values.


    " Usuários que podem ter acesso as condições de pagamento adicionais
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        class         = '0000'
        setnr         = 'MAGGI_ZSDT0044_04'
      TABLES
        set_values    = t_usermd
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    SORT t_usermd BY from.
    READ TABLE t_usermd WITH KEY from = sy-uname.
    IF sy-subrc = 0.
      values-text = 'Venda Futura'.
      values-key  = 'VF'.
      APPEND values.

      values-text = 'Bonificação'.
      values-key  = 'BN'.
      APPEND values.

      values-text = 'Permuta'.
      values-key  = 'PM'.
      APPEND values.
    ENDIF.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
    values_cpg_ant[] = values[].
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = 'WG_HEADER-TPSIM'
        values          = values[]
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.


    ENDIF.


    init = c_x.

  ENDIF.


* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  IF wg_save EQ c_altcpg.
    REFRESH values_cpg.

    IF wg_header-tpsim EQ 'VV'.
      values_cpg-text = 'Venda A Vista'.
      values_cpg-key  = 'VV'.
      APPEND values_cpg.
    ENDIF.

    IF wg_header-tpsim EQ 'VP'.
      values_cpg-text = 'Venda A Prazo'.
      values_cpg-key  = 'VP'.
      APPEND values_cpg.
    ENDIF.

    values_cpg-text = 'Adiantamento'.
    values_cpg-key  = 'AD'.
    APPEND values_cpg.

    values_cpg-text = 'Troca Safra'.
    values_cpg-key  = 'TS'.
    APPEND values_cpg.

    values_cpg-text = 'Troca a Vista'.
    values_cpg-key  = 'TV'.
    APPEND values_cpg.

  ELSE.
    values_cpg[] = values_cpg_ant[].
  ENDIF.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'WG_HEADER-TPSIM'
      values          = values_cpg[]
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8


*Início - Sara Oikawa - 38859 - Agosto/2020
  " Valores Drop-Down para Meio Pagamento
  REFRESH values.
  IF wg_header-tpsim NE 'VV'.
    values-text = 'Acerto'.
    values-key  = 'A'.
    APPEND values.
  ENDIF.

*  values-text = 'Boleto Bancário'.
*  values-key  = 'B'.
*  APPEND values.
*
*  values-text = 'Depósito em Conta'.
*  values-key  = 'D'.
*  APPEND values.

  values-text = 'Boleto Bancário'.
  values-key  = 'D'.
  APPEND values.

  values-text = 'Depósito em Conta'.
  values-key  = 'U'.
  APPEND values.

  " 21.02.2023 - RAMON - 102323 -->
  IF wg_header-tpsim = 'VV' AND wg_header-ecommerce = 'X'.

    DELETE values WHERE key = 'U'.

    values-text = 'PIX'.
    values-key  = 'X'.
    APPEND values.

    "23-02-2024 - SMC - 134596
    values-text = 'Financiamento'.
    values-key  = 'F'.
    APPEND values.
    "23-02-2024 - SMC - 134596

  ENDIF.
  " 21.02.2023 - RAMON - 102323 --<

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'WG_MEIO_PAGO'
      values          = values[]
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

*Fim - Sara Oikawa - 38859 - Agosto/2020


  SELECT SINGLE *
  FROM zsdt0040
  INTO wl_0040
   WHERE doc_simulacao EQ wg_header-doc_simulacao.
  LOOP AT SCREEN.
    IF ( screen-name EQ 'PB_PAGTO' ).
      "ALRS
*Início - Sara Oikawa - 38859 - Agosto/2020
*      IF WG_HEADER-MEIO_PAGO EQ 'B'
      IF ( wg_meio_pago EQ 'D'

        " 07.03.2023 - RAMON - E-Commerce -->
        OR wg_meio_pago EQ 'X'
        " 07.03.2023 - RAMON - E-Commerce --<

        OR wg_meio_pago EQ 'F' )"23-02-2024 - SMC - 134596

*Fim - Sara Oikawa - 38859 - Agosto/2020
        AND ( wg_header-tpsim = 'VV' OR wg_header-tpsim = 'VP' )  "WSBB
        AND wg_header-waerk = 'BRL'
        AND wl_0040-status = 'A'.

        screen-invisible = 0.
        MODIFY SCREEN.
      ELSE.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

*** Adiantamento
  IF wg_header-tpsim(1) EQ c_a.
    LOOP AT SCREEN.

      IF screen-group1 EQ c_tro
      OR screen-group1 EQ c_vis.
        IF screen-name NE 'WG_HEADER-VLRTOT'
        OR screen-name NE 'WG_HEADER-TPCULT'
        OR screen-name NE 'WG_HEADER-FCULT'
        OR screen-name NE 'WG_HEADER-DTENT'.
          ASSIGN (screen-name) TO <fs_campo>.
          IF <fs_campo> IS ASSIGNED.
            CLEAR: <fs_campo>.
          ENDIF.
          UNASSIGN <fs_campo>.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF screen-name = 'WG_HEADER-ANTEC'
      OR screen-name = 'WG_HEADER-DTINIJUROS'
      OR screen-name = 'WG_HEADER-KURSF'.
        ASSIGN (screen-name) TO <fs_campo>.
        IF <fs_campo> IS ASSIGNED.
          CLEAR: <fs_campo>.
        ENDIF.
        UNASSIGN <fs_campo>.

        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.

      IF ( screen-name EQ 'WG_HEADER-DTVENCOV' ).
        CASE wg_header-tpsim.
          WHEN 'AD' OR 'VV'.
          WHEN OTHERS.
            screen-active = 0.
            MODIFY SCREEN.
        ENDCASE.

*        IF WG_HEADER-TPSIM NE 'AD' OR WG_HEADER-TPSIM NE 'VV'.
*          SCREEN-ACTIVE = 0.
*          MODIFY SCREEN.
*        ENDIF.
      ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
*      IF ( SCREEN-NAME EQ 'WG_HEADER-MEIO_PAGO' ).
      IF ( screen-name EQ 'WG_MEIO_PAGO' ).
*Fim - Sara Oikawa - 38859 - Agosto/2020
        IF wg_header-tpsim NE 'VV' AND wg_header-tpsim NE 'VP'. "WSBB
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

*** US - 81799 - Inicio - Cbrand
      IF ( screen-name EQ 'WG_HEADER-HBKID' ) .
        IF ( wg_header-tpsim NE 'VV' AND wg_header-tpsim NE 'VP' ) AND wg_meio_pago <> 'D'.
          screen-active = 0.
          screen-input  = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
      IF ( screen-name EQ 'WG_DESC_HBKID' ) OR ( screen-name EQ 'WG_HEADER-HBKID').
        IF ( wg_header-tpsim EQ 'VV' OR wg_header-tpsim EQ 'VP' ) AND wg_meio_pago = 'D'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
*** US - 81799 - Fim - Cbrand
    ENDLOOP.

    CALL METHOD grid1->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = t_fieldcatalog.

    LOOP AT t_fieldcatalog INTO w_fieldcatalog.
      IF w_fieldcatalog-fieldname EQ 'CALCU'
      OR w_fieldcatalog-fieldname EQ 'TRUNIT'
      OR w_fieldcatalog-fieldname EQ 'TRTOT'.

        w_fieldcatalog-no_out = c_x.
      ELSE.
        w_fieldcatalog-no_out = space.

      ENDIF.

      MODIFY t_fieldcatalog FROM w_fieldcatalog.
      CLEAR: w_fieldcatalog.
    ENDLOOP.

    CALL METHOD grid1->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog.

** Troca
  ELSEIF wg_header-tpsim(1) EQ c_t.

    LOOP AT SCREEN.
      IF screen-group1 EQ c_adt
      OR screen-group1 EQ c_vis.
        IF screen-name NE 'WG_HEADER-VLRTOT'.
          IF screen-name NE 'WG_DESC_DAT'.
            ASSIGN (screen-name) TO <fs_campo>.
            IF <fs_campo> IS ASSIGNED.
              CLEAR: <fs_campo>.
            ENDIF.
            UNASSIGN <fs_campo>.
          ENDIF.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF screen-name = 'WG_HEADER-KURSF'.
        ASSIGN (screen-name) TO <fs_campo>.
        IF <fs_campo> IS ASSIGNED.
          CLEAR: <fs_campo>.
        ENDIF.
        UNASSIGN <fs_campo>.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name = 'WG_HEADER-ANTEC'.
        IF wg_header-tpsim+1(1) EQ 'V'.
          IF screen-name NE 'WG_DESC_DAT'.
            ASSIGN (screen-name) TO <fs_campo>.
            IF <fs_campo> IS ASSIGNED.
              CLEAR: <fs_campo>.
            ENDIF.
            UNASSIGN <fs_campo>.
          ENDIF.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
*      IF ( SCREEN-NAME EQ 'WG_HEADER-MEIO_PAGO' ).
      IF ( screen-name EQ 'WG_MEIO_PAGO' ).
*Fim - Sara Oikawa - 38859 - Agosto/2020
        IF wg_header-tpsim NE 'VV' AND wg_header-tpsim NE 'VP'. "WSBB
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

*** US - 81799 - Inicio - Cbrand
      IF ( screen-name EQ 'WG_HEADER-HBKID' ) OR ( screen-name EQ 'WG_DESC_HBKID' ) .
        IF wg_header-tpsim NE 'VV' AND wg_header-tpsim NE 'VP'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
      IF ( screen-name EQ 'WG_DESC_HBKID' ) .
        IF wg_header-tpsim EQ 'VV' OR wg_header-tpsim EQ 'VP'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

*** US - 81799 - Fim - Cbrand

      IF ( screen-name EQ 'WG_HEADER-DTVENCOV' ).
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.



    ENDLOOP.

    CALL METHOD grid1->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = t_fieldcatalog.

    LOOP AT t_fieldcatalog INTO w_fieldcatalog.
      IF w_fieldcatalog-fieldname EQ 'COMPR'.

        w_fieldcatalog-no_out = c_x.
      ELSE.
        w_fieldcatalog-no_out = space.
      ENDIF.
      MODIFY t_fieldcatalog FROM w_fieldcatalog.
      CLEAR: w_fieldcatalog.
    ENDLOOP.

    CALL METHOD grid1->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog.
** A Vista
  ELSEIF wg_header-tpsim(1) EQ c_v OR wg_header-tpsim(1) EQ c_p .

    LOOP AT SCREEN.
      IF screen-group1 EQ c_adt
      OR screen-group1 EQ c_tro.
        IF screen-name NE 'WG_HEADER-VLRTOT'.
          IF screen-name NE 'WG_DESC_DAT'.
            IF ( screen-name EQ 'WG_HEADER-JUROS_ANO'  AND wg_header-tpsim EQ 'VP') OR
               ( screen-name EQ 'WG_HEADER-JUROS_ANO'  AND ( wg_header-tpsim EQ 'PM'  OR          "Inclusão - Sara - 38859 - Agosto/2020
                                                             wg_header-tpsim EQ 'VF'  OR
                                                             wg_header-tpsim EQ 'VV' ) ) OR
               ( screen-name EQ 'WG_HEADER-DTINIJUROS' AND wg_header-tpsim EQ 'VP').

            ELSE.
              ASSIGN (screen-name) TO <fs_campo>.
              IF <fs_campo> IS ASSIGNED.
                CLEAR: <fs_campo>.
              ENDIF.
              UNASSIGN <fs_campo>.
            ENDIF.
          ENDIF.
          screen-active = 0.
          IF ( screen-name EQ 'WG_HEADER-JUROS_ANO' AND wg_header-tpsim EQ 'VP').
*            WG_DESC_DAT = 'Data Vencimento'.
            screen-active = 1.
          ENDIF.
          IF ( screen-name EQ 'WG_HEADER-DTINIJUROS' AND wg_header-tpsim EQ 'VP').
            screen-active = 1.
          ENDIF.
*Início - Sara Oikawa - 38859 - Agosto/2020
          IF ( screen-name EQ 'WG_HEADER-JUROS_ANO' AND wg_header-tpsim EQ 'PM').
            screen-active = 1.
          ENDIF.

          IF ( screen-name EQ 'WG_HEADER-JUROS_ANO' AND wg_header-tpsim EQ 'VF').
            screen-active = 1.
          ENDIF.

          IF ( screen-name EQ 'WG_HEADER-JUROS_ANO' AND wg_header-tpsim EQ 'VV').
            screen-active = 1.
          ENDIF.

*          IF ( SCREEN-NAME EQ 'WG_HEADER-DTINIJUROS' AND WG_HEADER-TPSIM EQ 'PM').
*            SCREEN-ACTIVE = 1.
*          ENDIF.

*Fim - Sara Oikawa - 38859 - Agosto/2020

          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF ( screen-name = 'WG_HEADER-DTPGTCULT' OR
           screen-name = 'WG_DESC_DAT'       ) AND
*         ( WG_HEADER-TPSIM NE 'VV' AND WG_HEADER-TPSIM NE 'VP' AND WG_HEADER-TPSIM NE 'PM' ).
          ( wg_header-tpsim NE 'VP' AND wg_header-tpsim NE 'PM' ).
        IF screen-name NE 'WG_DESC_DAT'.
          ASSIGN (screen-name) TO <fs_campo>.
          IF <fs_campo> IS ASSIGNED.
            CLEAR: <fs_campo>.
          ENDIF.
          UNASSIGN <fs_campo>.
        ENDIF.
        screen-active   = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name = 'WG_HEADER-KURSF' AND wg_header-tpsim NE 'VF'.
        ASSIGN (screen-name) TO <fs_campo>.
        IF <fs_campo> IS ASSIGNED.
          CLEAR: <fs_campo>.
        ENDIF.
        UNASSIGN <fs_campo>.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name = 'WG_HEADER-ANTEC'.
        IF screen-name NE 'WG_DESC_DAT'.
          ASSIGN (screen-name) TO <fs_campo>.
          IF <fs_campo> IS ASSIGNED.
            CLEAR: <fs_campo>.
          ENDIF.
          UNASSIGN <fs_campo>.
        ENDIF.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.

      IF ( screen-name EQ 'WG_HEADER-DTVENCOV' ).
        CASE wg_header-tpsim.
          WHEN 'AD' OR 'VV'.
          WHEN OTHERS.
            screen-active = 0.
            MODIFY SCREEN.
        ENDCASE.
*        IF WG_HEADER-TPSIM NE 'AD' OR WG_HEADER-TPSIM NE 'VV'.
*          SCREEN-ACTIVE = 0.
*          MODIFY SCREEN.
*        ENDIF.
      ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
*      IF ( SCREEN-NAME EQ 'WG_HEADER-MEIO_PAGO' ).
      IF ( screen-name EQ 'WG_MEIO_PAGO' ).
*Fim - Sara Oikawa - 38859 - Agosto/2020
        IF wg_header-tpsim NE 'VV' AND wg_header-tpsim NE 'VP'. "WSBB
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

*** US - 81799 - Inicio - Cbrand
      IF ( screen-name EQ 'WG_DESC_HBKID' ) .
        IF wg_header-tpsim EQ 'VV' OR wg_header-tpsim EQ 'VP'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
*** US - 81799 - Fim - Cbrand
** US - 81799 - Inicio - Cbrand
*      IF ( screen-name EQ 'WG_HEADER-HBKID' ).
*        IF wg_header-tpsim EQ 'VV' OR wg_header-tpsim EQ 'VP'.
*
*          ASSIGN (screen-name) TO <fs_campo>.
*          IF <fs_campo> IS ASSIGNED.
*            CLEAR: <fs_campo>.
*          ENDIF.
*          UNASSIGN <fs_campo>.
*
*          screen-active = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDIF.
    ENDLOOP.
** US - 81799 - Fim - Cbrand
    CALL METHOD grid1->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = t_fieldcatalog.


    LOOP AT t_fieldcatalog INTO w_fieldcatalog.
      IF w_fieldcatalog-fieldname EQ 'COMPR'
      OR w_fieldcatalog-fieldname EQ 'TRUNIT'
      OR w_fieldcatalog-fieldname EQ 'SIUMB'
      OR w_fieldcatalog-fieldname EQ 'TRTOT'
      OR w_fieldcatalog-fieldname EQ 'CALCU'.

        w_fieldcatalog-no_out = c_x.
      ELSE.
        w_fieldcatalog-no_out = space.
      ENDIF.
      MODIFY t_fieldcatalog FROM w_fieldcatalog.
      CLEAR: w_fieldcatalog.
    ENDLOOP.

    CALL METHOD grid1->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog.
  ELSE.
    LOOP AT SCREEN.

      IF screen-group1 EQ c_adt
      OR screen-group1 EQ c_tro
      OR screen-group1 EQ c_vis.
        IF  screen-name EQ 'WG_HEADER-VLRTOT' AND wg_header-tpsim EQ 'BN'.
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
      IF ( screen-name = 'WG_HEADER-DTPGTCULT' OR screen-name = 'WG_DESC_DAT' OR screen-name = 'WG_HEADER-ANTEC' ) AND wg_header-tpsim EQ 'BN'.
        IF screen-name NE 'WG_DESC_DAT'.
          ASSIGN (screen-name) TO <fs_campo>.
          IF <fs_campo> IS ASSIGNED.
            CLEAR: <fs_campo>.
          ENDIF.
          UNASSIGN <fs_campo>.
        ENDIF.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name = 'WG_HEADER-KURSF' AND wg_header-tpsim EQ 'BN'.
        ASSIGN (screen-name) TO <fs_campo>.
        IF <fs_campo> IS ASSIGNED.
          CLEAR: <fs_campo>.
        ENDIF.
        UNASSIGN <fs_campo>.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
*      IF ( SCREEN-NAME EQ 'WG_HEADER-MEIO_PAGO' ).
      IF ( screen-name EQ 'WG_MEIO_PAGO' ).
*Fim - Sara Oikawa - 38859 - Agosto/2020
        IF wg_header-tpsim NE 'VV' AND wg_header-tpsim NE 'VP'. "WSBB
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

*** US - 81799 - Inicio - Cbrand
      IF ( screen-name EQ 'WG_HEADER-HBKID' ) OR ( screen-name EQ 'WG_DESC_HBKID' ) .
        IF wg_header-tpsim NE 'VV' AND wg_header-tpsim NE 'VP'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF ( screen-name EQ 'WG_DESC_HBKID' ) .
        IF wg_header-tpsim EQ 'VV' OR wg_header-tpsim EQ 'VP'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
*** US - 81799 - Fim - Cbrand

    ENDLOOP.

  ENDIF.

  IF wg_acao EQ c_add
  OR wg_acao EQ c_modif.
    LOOP AT tg_itens WHERE vbeln IS NOT INITIAL.

    ENDLOOP.
    LOOP AT SCREEN.
* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
*      IF SY-SUBRC IS NOT INITIAL.
      IF sy-subrc IS NOT INITIAL OR wg_save EQ c_altcpg.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
        IF screen-group2 EQ 'A1'.
          IF screen-name EQ 'PB_DESCP'.
            screen-invisible = 0.
          ENDIF.
          screen-input = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
      IF screen-group2 EQ 'A2'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

      IF ( screen-name EQ 'WG_HEADER-DTVENCOV' ).
        CASE wg_header-tpsim.
          WHEN 'AD' OR 'VV'.
          WHEN OTHERS.
            screen-active = 0.
            MODIFY SCREEN.
        ENDCASE.
*        IF WG_HEADER-TPSIM NE 'AD' OR WG_HEADER-TPSIM NE 'VV'.
*          SCREEN-ACTIVE = 0.
*          MODIFY SCREEN.
*        ENDIF.
      ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
*      IF ( SCREEN-NAME EQ 'WG_HEADER-MEIO_PAGO' ).
      IF ( screen-name EQ 'WG_MEIO_PAGO' ).
*Fim - Sara Oikawa - 38859 - Agosto/2020
        IF wg_header-tpsim NE 'VV' AND wg_header-tpsim NE 'VP'. "WSBB
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

*** US - 81799 - Inicio - Cbrand
      IF ( screen-name EQ 'WG_HEADER-HBKID' ) OR ( screen-name EQ 'WG_DESC_HBKID' ) .
        IF wg_header-tpsim NE 'VV' AND wg_header-tpsim NE 'VP'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF ( screen-name EQ 'WG_DESC_HBKID' ) .
        IF wg_header-tpsim EQ 'VV' OR wg_header-tpsim EQ 'VP'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
*** US - 81799 - Fim - Cbrand

    ENDLOOP.

*** modificar aqui
    LOOP AT tg_itens WHERE vbeln IS NOT INITIAL.

    ENDLOOP.
    IF sy-subrc IS INITIAL.
      IF grid1 IS NOT INITIAL.

        CALL METHOD grid1->get_frontend_fieldcatalog
          IMPORTING
            et_fieldcatalog = t_fieldcatalog.

*      INSERT LINES OF STYLE INTO TABLE TG_ITENS-STYLE.
***         FIM DE BLOQUEI DE CAMPOS

        LOOP AT tg_itens.
          REFRESH: tg_itens-style.
          REFRESH: style.
          CLEAR: wa_style.
          LOOP AT t_fieldcatalog INTO w_fieldcatalog
           WHERE edit EQ c_x.
            wa_style-fieldname = w_fieldcatalog-fieldname.
            IF tg_itens-vbeln IS NOT INITIAL.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            ELSE.
              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
            ENDIF.
            INSERT  wa_style INTO TABLE style .
          ENDLOOP.
          INSERT LINES OF style INTO TABLE tg_itens-style.
          MODIFY tg_itens.
        ENDLOOP.

      ENDIF.
    ENDIF.

    LOOP AT SCREEN.
      LOOP AT tg_itens.
        CASE tg_itens-spart.
          WHEN '02'.
            IF screen-name EQ 'WG_HEADER-DT_ENTREGA_FET'.
              screen-input = 1.
            ENDIF.
          WHEN '03'.
            IF screen-name EQ 'WG_HEADER-DT_ENTREGA_DEF'.
              screen-input = 1.
            ENDIF.
          WHEN '04'.
            IF screen-name EQ 'WG_HEADER-DT_ENTREGA_SEM'.
              screen-input = 1.
            ENDIF.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
    ENDLOOP.


  ELSEIF wg_acao EQ c_descont.
    LOOP AT SCREEN.
      IF screen-group2 EQ 'A2'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CASE sy-ucomm.
      "WSB SCREEN SACA
    WHEN c_sacas.
      LOOP AT SCREEN.
        IF  screen-name EQ 'WG_HEADER-TROTOTSC'
         OR screen-name EQ 'CONVERT_TRATOTSC'.

          screen-input = 1.
          MODIFY SCREEN.

        ENDIF.
      ENDLOOP.

    WHEN c_dsc_abs.

      LOOP AT tg_itens.
        FREE: it_vbfa, it_vbap, it_41, it_vbak, it_90.

        SELECT *
          FROM vbfa
          INTO TABLE it_vbfa
          WHERE vbelv EQ tg_itens-vbeln.

        IF it_vbfa IS NOT INITIAL.
          FREE it_vbfa.

          SELECT *
            FROM vbak
            INTO TABLE it_vbak
            WHERE vbeln EQ tg_itens-vbeln.

          IF it_vbak IS NOT INITIAL.
            SELECT *
              FROM vbap
              INTO TABLE it_vbap
              FOR ALL ENTRIES IN it_vbak
              WHERE vbeln EQ it_vbak-vbeln.

            IF it_vbap IS NOT INITIAL.
              SELECT *
                FROM vbfa
                INTO TABLE it_vbfa
                FOR ALL ENTRIES IN it_vbap
                WHERE vbelv EQ it_vbap-vbeln.

              LOOP AT it_vbap INTO wa_vbap.
                READ TABLE it_vbfa TRANSPORTING NO FIELDS WITH KEY vbelv = wa_vbap-vbeln
                                                                   posnv = wa_vbap-posnr.
                IF sy-subrc IS INITIAL.
                  SELECT *
                  FROM zsdt0041
                  APPENDING TABLE it_41
                    WHERE doc_simulacao EQ wg_header-doc_simulacao
                      AND posnr EQ tg_itens-posnr
                      AND matnr EQ wa_vbap-matnr.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.

        SELECT *
          FROM zsdt0090
            APPENDING TABLE it_90
              WHERE vbelv EQ tg_itens-vbeln.

        CLEAR: wa_style.
        REFRESH: tg_itens-style.
        wa_style-fieldname = 'DESC_ABSOLUTO'.


        IF it_41 IS NOT INITIAL OR it_90[] IS NOT INITIAL.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        ELSE.
          wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
        ENDIF.


        READ TABLE style TRANSPORTING NO FIELDS WITH KEY fieldname = 'DESC_ABSOLUTO'.
        IF sy-subrc IS INITIAL.
          DELETE style WHERE fieldname = 'DESC_ABSOLUTO'.
          INSERT  wa_style INTO TABLE style.
        ELSE.
          INSERT  wa_style INTO TABLE style.
        ENDIF.

        INSERT LINES OF style INTO TABLE tg_itens-style.
        MODIFY tg_itens.

      ENDLOOP.

*Início - Sara Oikawa - 38859 - Agosto/2020
    WHEN c_altjur.
      LOOP AT SCREEN.
        IF  screen-name EQ 'WG_HEADER-JUROS_ANO'.
          screen-input = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN c_altadt.
      LOOP AT SCREEN.
        IF  screen-name EQ 'WG_HEADER-VLR_ADTO' OR
            screen-name EQ 'WG_HEADER-ADTO_HA'.
          screen-input = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

*Fim - Sara Oikawa - 38859 - Agosto/2020

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
    WHEN c_altcpg.
      LOOP AT SCREEN.
        IF  screen-name EQ 'WG_HEADER-TPSIM'.
          screen-input = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

  ENDCASE.

  IF wg_acao IS INITIAL OR s_acao EQ 'S_ATU'.
    LOOP AT SCREEN.
      IF  screen-name EQ 'WG_HEADER-DOC_SIMULACAO'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  IF wg_acao EQ c_modif AND wg_save EQ c_altcpg.

    IF wg_header-tpsim(1) EQ c_tro(1).
      LOOP AT SCREEN.

        IF screen-group1 NE c_tro.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.

        IF screen-name = 'WG_HEADER-TPSIM' OR
           screen-name = 'WG_HEADER-DTPGTCULT'.
          screen-input = 1.
          MODIFY SCREEN.
        ENDIF.

      ENDLOOP.
    ENDIF.

    IF wg_header-tpsim(1) EQ c_adt(1).
      LOOP AT SCREEN.
        IF screen-group1 NE c_adt.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.

        IF screen-name = 'WG_HEADER-TPSIM'    OR
           screen-name = 'WG_HEADER-DTVENCOV' OR
           screen-name = 'WG_HEADER-DTPGTCULT'.
          screen-input = 1.
          MODIFY SCREEN.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDIF.

* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8


* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  " A escolha das informações SIGAM se dará apenas pelas ações de NOVO / COPIAR / EDITAR
  " Ou Seja, o Simulador deverá estar com um Status que permita a edição das informações como um todo
  IF ( wl_0040-status = c_a AND wg_save NE c_altcpg ) OR     "Pacote8
    ( wg_acao NE c_add AND wg_acao NE c_copy AND wg_acao NE c_modif ).
    LOOP AT SCREEN.
      IF screen-name EQ c_btn_sg.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

**Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
*  IF wl_0040-status = c_a and ( wg_acao EQ c_modif and wg_save = c_altcpg ).
*    LOOP AT SCREEN.
*      IF screen-name EQ c_btn_sg.
*        screen-active = 1.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
**Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

*  " 21.02.2023 - RAMON - 102323 -->
  LOOP AT SCREEN.

    CHECK screen-name = 'WG_HEADER-ID_ORDER_ECOMMERCE'.

    IF wg_header-ecommerce = 'X'.
      "screen-input = 1.
    ELSE.

      IF wg_acao = c_add OR wg_acao = c_modif.
        screen-input = 0.
      ENDIF.

    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.
*
*  " 21.02.2023 - RAMON - 102323 --<

ENDMODULE.                 " MODIFY_SCREEN_0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_FRETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_frete CHANGING p_vlr_frete.

  CLEAR taxa_fre.

  SELECT SINGLE kursk
    FROM zsdt0117
    INTO taxa_fre
    WHERE bukrs EQ wa_t001k-bukrs
    AND desativado NE abap_true.

  wg_header-taxa_frete = taxa_fre.

  CHECK wg_header-waerk EQ 'USD'.
*RSI - Case #CS0971604 Ajuste divisão frete
  IF taxa_fre NE 0.
    p_vlr_frete = p_vlr_frete / taxa_fre.
  ELSE.
    CLEAR p_vlr_frete.
    erro_taxa_frete = wa_t001k-bukrs.
  ENDIF.
*RSI - Case #CS0971604 Ajuste divisão frete
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALCULA_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calcula_header USING wl_tot_vlr.

  IF ( wg_header-tpsim(1) EQ c_adt(1)
  OR   wg_header-tpsim(1) EQ c_b     ).

    CLEAR: wl_tot_vlr, wg_header-vlrtot, wg_header-area_penhor, wg_header-comprsc.

    PERFORM vlrtot.

    LOOP AT tg_itens INTO wl_itens.
      ADD wl_itens-vlrtot TO wl_tot_vlr.
**    Realiza o calculo do campo "Valor Total"
*      ADD WL_ITENS-VLRTOT TO WG_HEADER-VLRTOT.
**    Realiza o calculo do campo "Compromisso/sac"
      ADD wl_itens-compr  TO wg_header-comprsc.
    ENDLOOP.
**    Realiza o calculo do campo "Area de Penhor"
    TRY.
        wg_header-area_penhor = wl_tot_vlr / wg_header-adto_ha.
      CATCH cx_sy_zerodivide.
    ENDTRY.


  ELSEIF ( wg_header-tpsim(1) EQ c_tro(1) ).

    CLEAR: wg_header-trototsc, wg_header-scha, wg_header-vlrtot, convert_tratotsc.

    PERFORM vlrtot.

    LOOP AT tg_itens INTO wl_itens.

*    Realiza o calculo do campo "Troca Total Sc"
      ADD: wl_itens-trtot TO wg_header-trototsc,
           wl_itens-trtot TO convert_tratotsc.

**    Realiza o calculo do campo "Valor Total"
*      ADD WL_ITENS-VLRTOT TO WG_HEADER-VLRTOT.

    ENDLOOP.
    PERFORM monta_memo.

    PERFORM vlrtot.

***   Calculo Sc/Há
    TRY.
        wg_header-scha = wg_header-trototsc / wg_area_ha. "WG_HEADER-AREA_HA.
      CATCH cx_sy_zerodivide.
    ENDTRY.
  ELSE.
*    Realiza o calculo do campo "Valor Total"
    PERFORM vlrtot.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VLRTOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vlrtot .
**    Realiza o calculo do campo "Valor Total"

  CLEAR: wg_header-vlrtot.

  LOOP AT tg_itens INTO wl_itens.
    ADD wl_itens-vlrtot TO wg_header-vlrtot.
  ENDLOOP.

*  PERFORM VLR_AJUSTE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  DESBLOQUEIO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE desbloqueio INPUT.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  VINC_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vinc_desc.

  CLEAR: desconto, total, desc_acres.

  LOOP AT tg_trans ASSIGNING FIELD-SYMBOL(<trans>)
    WHERE categoria EQ 'O'
    AND flag EQ abap_false
    AND estorno NE abap_true.

    <trans>-flag = abap_true.
    ADD <trans>-desc_absoluto TO desconto.

  ENDLOOP.

  ADD wg_header-vlrtot TO total.
  ADD desconto TO total.

  IF  desconto > 0.
    desc_acres = 'Acrescimo:'.
  ELSE.
    desc_acres = 'Desconto:'.
  ENDIF.

  CALL SCREEN 0107 ENDING AT 51 7 STARTING AT 3 3.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  PBO0107  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo0107 OUTPUT.
  SET PF-STATUS 'PF0107'.
  SET TITLEBAR  'TI0107'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI0107  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai0107 INPUT.

  DATA: t_0090 TYPE TABLE OF zsdt0090 WITH HEADER LINE.

  CASE sy-ucomm.
    WHEN 'SAIR'.
      LEAVE TO SCREEN 0.
    WHEN 'APLICAR'.

      LOOP AT tg_trans INTO DATA(w0090).
        MOVE-CORRESPONDING w0090 TO t_0090.
        APPEND t_0090.
        CLEAR t_0090.
      ENDLOOP.

      CHECK NOT t_0090[] IS INITIAL.

      MODIFY zsdt0090 FROM TABLE t_0090.

      CHECK NOT wg_header-doc_simulacao IS INITIAL.

      UPDATE zsdt0040 SET vlrtot = total
      WHERE doc_simulacao EQ wg_header-doc_simulacao.

      LEAVE TO SCREEN 0.

    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_IMPOSTO
*&---------------------------------------------------------------------*
FORM busca_imposto CHANGING p_wl_itens LIKE LINE OF tg_itens.

  DATA: wa_regio_k   TYPE kna1-regio,
        wa_regio_l   TYPE lfa1-regio,
        vl_lifnr     TYPE lfa1-lifnr,
        wa_zsdt0008  TYPE zsdt0008,
        wa_j_1btxic3 TYPE j_1btxic3,
        wa_ownpr     TYPE mbew-ownpr.

  CLEAR: p_wl_itens-vlr_icms,
         p_wl_itens-vlr_cofins ,
         p_wl_itens-vlr_pis,
         p_wl_itens-j_1btxsdc,
         p_wl_itens-j_1btaxlw1,
         p_wl_itens-j_1btaxlw5,
         p_wl_itens-j_1btaxlw4,
         p_wl_itens-zwert_liqdo.

  vl_lifnr = |{ p_wl_itens-werks ALPHA = IN }|.

  SELECT SINGLE regio
    FROM kna1
    INTO wa_regio_k
    WHERE kunnr EQ wg_header-kunnr.

  SELECT SINGLE regio
    FROM lfa1
    INTO wa_regio_l
    WHERE lifnr EQ vl_lifnr.

  SELECT SINGLE ownpr
    FROM mbew
    INTO wa_ownpr
    WHERE matnr EQ p_wl_itens-matnr
      AND bwkey EQ p_wl_itens-werks.



  SELECT SINGLE *
    FROM zsdt0008
    INTO wa_zsdt0008
   WHERE auart      EQ p_wl_itens-auart
     AND vkaus      EQ 'I'
     AND mwsk1      EQ 'SD'
     AND uf_centro  EQ wa_regio_l
     AND uf_cliente EQ wa_regio_k
     AND ownpr      EQ wa_ownpr.

  zcl_util_sd=>conv_data_us_br( EXPORTING i_data = sy-datum
                            RECEIVING e_data = _hoje_invdt ).

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = _hoje_invdt
    IMPORTING
      output = _hoje_invdt.

  IF wa_zsdt0008 IS NOT INITIAL.

    p_wl_itens-j_1btxsdc  = wa_zsdt0008-j_1btxsdc.
    p_wl_itens-j_1btaxlw5 = wa_zsdt0008-j_1btaxlw5.
    p_wl_itens-j_1btaxlw4 = wa_zsdt0008-j_1btaxlw4.


    SELECT COUNT(*)
      FROM knvi
      UP TO 1 ROWS
      WHERE kunnr EQ wg_header-kunnr
      AND tatyp EQ 'IBRX'
      AND taxkd EQ '2'.

    IF sy-subrc IS INITIAL.

      p_wl_itens-zwert_liqdo = p_wl_itens-zwert.

    ELSE.

      SELECT COUNT(*)
        FROM j_1btxsdc
      WHERE taxcode EQ wa_zsdt0008-j_1btxsdc
        AND custusage EQ '1'
        AND icms EQ abap_true.

      IF sy-subrc IS INITIAL.

        SELECT SINGLE *
           FROM j_1btxic3
           INTO wa_j_1btxic3
          WHERE land1     EQ 'BR'
            AND shipfrom  EQ wa_regio_l
            AND shipto    EQ wa_regio_k
            AND gruop     EQ '76'
            AND value     EQ wg_header-kunnr
            AND value2    EQ p_wl_itens-matnr
            AND validfrom GE _hoje_invdt
            AND validto   LE _hoje_invdt.


        IF sy-subrc IS NOT INITIAL.

          SELECT SINGLE extwg
            FROM mara
            INTO @DATA(grupo_merc_ext)
            WHERE matnr EQ @p_wl_itens-matnr.

          IF grupo_merc_ext IS NOT INITIAL.
* RIM - SKM - IR121669 - Início
            SELECT SINGLE *
                  FROM j_1btxic3
                  INTO wa_j_1btxic3
                 WHERE land1    EQ 'BR'
                   AND shipfrom EQ wa_regio_l
                   AND shipto   EQ wa_regio_k
                   AND gruop    EQ '78'
                   AND value    EQ grupo_merc_ext
                   AND value2   EQ p_wl_itens-werks
                   AND validfrom GE _hoje_invdt  ">=
                   AND validto   LE _hoje_invdt. "<=
            IF sy-subrc IS NOT INITIAL.
* RIM - SKM - IR121669 - Fim
              SELECT SINGLE *
                    FROM j_1btxic3
                    INTO wa_j_1btxic3
                   WHERE land1    EQ 'BR'
                     AND shipfrom EQ wa_regio_l
                     AND shipto   EQ wa_regio_k
                     AND gruop    EQ '79'
                     AND value    EQ grupo_merc_ext
                     AND validfrom GE _hoje_invdt  ">=
                     AND validto   LE _hoje_invdt. "<=
            ENDIF.  "<<RIM - SKM - IR120631
            IF sy-subrc IS NOT INITIAL.

              SELECT SINGLE *
                          FROM j_1btxic3
                          INTO wa_j_1btxic3
                         WHERE land1    EQ 'BR'
                           AND shipfrom EQ wa_regio_l
                           AND shipto   EQ wa_regio_k
                           AND gruop    EQ '77'
                           AND value    EQ p_wl_itens-matnr
                           AND validfrom GE _hoje_invdt  ">=
                           AND validto   LE _hoje_invdt. "<=
            ENDIF.

          ELSE.

            SELECT SINGLE *
                  FROM j_1btxic3
                  INTO wa_j_1btxic3
                 WHERE land1    EQ 'BR'
                   AND shipfrom EQ wa_regio_l
                   AND shipto   EQ wa_regio_k
                   AND gruop    EQ '77'
                   AND value    EQ p_wl_itens-matnr
                   AND validfrom GE _hoje_invdt  ">=
                   AND validto   LE _hoje_invdt. "<=
*             AND TAXLAW   EQ WA_ZSDT0008-J_1BTAXLW1.

          ENDIF.
        ENDIF.

        p_wl_itens-j_1btaxlw1 = wa_j_1btxic3-taxlaw.

        p_wl_itens-vlr_icms = ( p_wl_itens-zwert * wa_j_1btxic3-base / 100 ) * ( wa_j_1btxic3-rate / 100 ).
        p_wl_itens-zwert_liqdo = p_wl_itens-zwert - p_wl_itens-vlr_icms - p_wl_itens-vlr_cofins - p_wl_itens-vlr_pis.

        IF p_wl_itens-zwert_liqdo IS INITIAL.
          p_wl_itens-zwert_liqdo = p_wl_itens-zwert.
        ENDIF.

      ELSE.
        p_wl_itens-zwert_liqdo = p_wl_itens-zwert.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SALVA_IMPOSTO
*&---------------------------------------------------------------------*
FORM salva_imposto.

  DATA: vl_index TYPE i.

  LOOP AT tg_itens.

    vl_index = sy-tabix.
    PERFORM busca_imposto CHANGING tg_itens.
    MODIFY tg_itens[] FROM tg_itens INDEX vl_index TRANSPORTING vlr_icms zwert_liqdo.
    CLEAR: tg_itens.

  ENDLOOP.

  LOOP AT tg_itens.

    UPDATE zsdt0041 SET vlr_icms = tg_itens-vlr_icms
                     zwert_liqdo = tg_itens-zwert_liqdo
                  WHERE doc_simulacao = wg_header-doc_simulacao
                    AND posnr = tg_itens-posnr.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  IMPRIME_OV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_ov.

  SELECT * FROM zsdt0041
    INTO TABLE @DATA(it_0041_)
    WHERE doc_simulacao EQ @wg_header-doc_simulacao.

  CALL FUNCTION 'Z_GERA_OV_CONTRATO'
    EXPORTING
      i_vlr = vlr
    TABLES
      vbeln = it_0041_.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS_IMPOSTOS
*&---------------------------------------------------------------------*
FORM verifica_erros_impostos.

  DATA: wa_regio_k   TYPE kna1-regio,
        it_lfa1      TYPE STANDARD TABLE OF lfa1,
        it_mbew      TYPE STANDARD TABLE OF mbew,
        wa_mbew      TYPE mbew,
        wa_lfa1      TYPE lfa1,
        wa_zsdt0008  TYPE zsdt0008,
        wa_j_1btxic3 TYPE j_1btxic3,
        vl_lifnr     TYPE lfa1-lifnr,
        wl_linha(6).

*  REFRESH: TG_MSG_RET.
*  CLEAR: TG_MSG_RET.

  SELECT SINGLE regio
    FROM kna1
    INTO wa_regio_k
    WHERE kunnr EQ wg_header-kunnr.

  SELECT *
    FROM mbew
    INTO TABLE it_mbew
    FOR ALL ENTRIES IN tg_itens
    WHERE matnr EQ tg_itens-matnr
      AND bwkey EQ tg_itens-werks.

  zcl_util_sd=>conv_data_us_br( EXPORTING i_data = sy-datum
                                RECEIVING e_data = _hoje_invdt ).

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = _hoje_invdt
    IMPORTING
      output = _hoje_invdt.

  LOOP AT tg_itens.

    wl_linha = sy-tabix.
    CLEAR: wa_mbew, wa_zsdt0008, wa_j_1btxic3, wa_lfa1, vl_lifnr.

    vl_lifnr = |{ tg_itens-werks ALPHA = IN }|.

    SELECT SINGLE *
      FROM lfa1
      INTO wa_lfa1
      WHERE lifnr EQ vl_lifnr.

    IF wa_lfa1 IS NOT INITIAL.

      READ TABLE it_mbew INTO wa_mbew WITH KEY matnr = tg_itens-matnr
                                               bwkey = tg_itens-werks.

      SELECT SINGLE *
         FROM zsdt0008
         INTO wa_zsdt0008
        WHERE auart      EQ tg_itens-auart
          AND vkaus      EQ 'I'
          AND mwsk1      EQ 'SD'
          AND uf_centro  EQ wa_lfa1-regio
          AND uf_cliente EQ wa_regio_k
          AND ownpr      EQ wa_mbew-ownpr.

      IF wa_zsdt0008 IS INITIAL.
        "Falta parâmetros de cadastro na trasação ZSDT0011
        MOVE: 'VLR_ICMR' TO tg_msg_ret-field,
              'GRID1'    TO tg_msg_ret-obj,
              wl_linha  TO tg_msg_ret-tabix.

        CONCATENATE TEXT-e48 ' LINHA: ' wl_linha INTO tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSE.

        SELECT SINGLE *
                       FROM j_1btxic3
                       INTO wa_j_1btxic3
                      WHERE land1     EQ 'BR'
                        AND shipfrom  EQ wa_lfa1-regio
                        AND shipto    EQ wa_regio_k
                        AND gruop     EQ '76'
                        AND value     EQ wg_header-kunnr
                        AND value2    EQ tg_itens-matnr
                        AND validfrom GE _hoje_invdt
                        AND validto   LE _hoje_invdt.

        IF sy-subrc IS NOT INITIAL.

          SELECT COUNT(*)
            FROM knvi
            UP TO 1 ROWS
            WHERE kunnr EQ wg_header-kunnr
            AND tatyp EQ 'IBRX'
            AND taxkd EQ '2'.

          IF sy-subrc IS INITIAL.
*                   "//Suframa
*                    SELECT SINGLE *
*                          FROM J_1BTXIC3
*                          INTO WA_J_1BTXIC3
*                         WHERE LAND1     EQ 'BR'
*                           AND SHIPFROM  EQ WA_LFA1-REGIO
*                           AND SHIPTO    EQ WA_REGIO_K
*                           AND GRUOP     EQ '76'
*                           AND VALUE     EQ WG_HEADER-KUNNR
*                           AND VALUE2    EQ TG_ITENS-MATNR
*                           AND VALIDFROM GE _HOJE_INVDT
*                           AND VALIDTO   LE _HOJE_INVDT.

*                    IF SY-SUBRC IS NOT INITIAL.
            "Não foi localizado cadastro da Suframa
            APPEND VALUE #(
                            field = 'VLR_ICMS'
                            obj = 'GRID1'
                            tabix = wl_linha
                            msg = |Cliente Suframa! { TEXT-e46 }  LINHA: { wl_linha } |
                          ) TO tg_msg_ret.
*                    ENDIF.

          ELSE.

            IF wa_lfa1-regio NE wa_regio_k.

              SELECT SINGLE extwg
                  FROM mara
                 INTO @DATA(grupo_merc_ext)
                  WHERE matnr EQ @tg_itens-matnr.

              IF grupo_merc_ext IS NOT INITIAL.
* RIM - SKM - IR120631 - Início
                SELECT SINGLE *
                      FROM j_1btxic3
                      INTO wa_j_1btxic3
                     WHERE land1    EQ 'BR'
                       AND shipfrom EQ wa_lfa1-regio
                       AND shipto   EQ wa_regio_k
                       AND gruop    EQ '78'
                       AND value    EQ grupo_merc_ext
                       AND value2   EQ tg_itens-werks
                       AND validfrom GE _hoje_invdt  ">=
                       AND validto   LE _hoje_invdt. "<=
                IF sy-subrc IS NOT INITIAL.
* RIM - SKM - IR120631 - Fim
                  SELECT SINGLE *
                        FROM j_1btxic3
                        INTO wa_j_1btxic3
                       WHERE land1    EQ 'BR'
                         AND shipfrom EQ wa_lfa1-regio
                         AND shipto   EQ wa_regio_k
                         AND gruop    EQ '79'
                         AND value    EQ grupo_merc_ext
                         AND validfrom GE _hoje_invdt  ">=
                         AND validto   LE _hoje_invdt. "<=
                ENDIF.  "<<RIM - SKM - IR120631

                IF sy-subrc IS NOT INITIAL.

                  SELECT SINGLE *
                              FROM j_1btxic3
                              INTO wa_j_1btxic3
                             WHERE land1    EQ 'BR'
                               AND shipfrom EQ wa_lfa1-regio
                               AND shipto   EQ wa_regio_k
                               AND gruop    EQ '77'
                               AND value    EQ tg_itens-matnr
                               AND validfrom GE _hoje_invdt  ">=
                               AND validto   LE _hoje_invdt. "<=
                ENDIF.

              ELSE.

                SELECT SINGLE *
                           FROM j_1btxic3
                           INTO wa_j_1btxic3
                          WHERE land1    EQ 'BR'
                            AND shipfrom EQ wa_lfa1-regio
                            AND shipto   EQ wa_regio_k
                            AND gruop    EQ '77'
                            AND value    EQ tg_itens-matnr
                            AND validfrom GE _hoje_invdt  ">=
                            AND validto   LE _hoje_invdt. "<=
              ENDIF.

              IF wa_j_1btxic3 IS INITIAL.
                "Não foi localizado cadastro de Excessão Dinâmica de ICMS
                MOVE: 'VLR_ICMS' TO tg_msg_ret-field,
                      'GRID1'    TO tg_msg_ret-obj,
                       wl_linha  TO tg_msg_ret-tabix.

                CONCATENATE TEXT-e46 ' LINHA: ' wl_linha INTO tg_msg_ret-msg.
                APPEND tg_msg_ret.
                CLEAR: tg_msg_ret.
              ELSE.
                "Não faz nada, tem cadastro.
              ENDIF.

            ELSE.
              "Não faz, domicílios no mesmo estado.
            ENDIF.

          ENDIF.
        ENDIF.

      ENDIF.

    ELSE.
      "Não localizado UF para o Centro Fornecedor
      MOVE: 'WAERS' TO tg_msg_ret-field,
            'GRID1' TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE TEXT-e47 ' LINHA: ' wl_linha INTO tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALCULA_AJUSTES_ORDEM
*&---------------------------------------------------------------------*
FORM calcula_ajustes_ordem.

  DATA: wa_zsdt0041_aux    TYPE zsdt0041,
        wa_zsdt0041_ajuste TYPE zsdt0041,
        it_zsdt0041_aux    TYPE STANDARD TABLE OF zsdt0041,
        vl_arred_liq       TYPE zsdt0041-zwert_liqdo,
        vl_arred_tx        TYPE zsdt0041-zwert_liqdo.

  CLEAR: it_zsdt0041_ajuste.

  SELECT *
    FROM zsdt0041
    INTO TABLE it_zsdt0041_aux
    WHERE doc_simulacao EQ wg_header-doc_simulacao.

  LOOP AT it_zsdt0041_aux INTO wa_zsdt0041_aux.

    wa_zsdt0041_ajuste-doc_simulacao = wg_header-doc_simulacao.
    wa_zsdt0041_ajuste-vbeln         = wa_zsdt0041_aux-vbeln.
    wa_zsdt0041_ajuste-posnr         = wa_zsdt0041_aux-posnr.
    wa_zsdt0041_ajuste-matnr         = wa_zsdt0041_aux-matnr.

    SELECT SINGLE knumv
      FROM vbak
      INTO @DATA(vl_knumv)
      WHERE vbeln EQ @wa_zsdt0041_aux-vbeln.

    SELECT SINGLE posnr
      FROM vbap
      INTO @DATA(vl_posnr)
      WHERE vbeln EQ @wa_zsdt0041_aux-vbeln
        AND matnr EQ @wa_zsdt0041_aux-matnr.


*---> 19/07/2023 - Migração S4 - DG
*    SELECT SINGLE kbetr
*      FROM konv
*      INTO @DATA(vl_kbetr)
*      WHERE knumv EQ @vl_knumv
*        AND kposn EQ @vl_posnr
*        AND kschl EQ 'RB00'.

    SELECT SINGLE kbetr
      FROM v_konv
      INTO @DATA(vl_kbetr)
      WHERE knumv EQ @vl_knumv
        AND kposn EQ @vl_posnr
        AND kschl EQ 'RB00'.
*<--- 19/07/2023 - Migração S4 - DG

* Arredondando parte do valor líquido do ajuste

    vl_arred_liq = wa_zsdt0041_aux-zwert_liqdo.

    CALL FUNCTION 'ROUND'
      EXPORTING
        decimals = 2
        input    = vl_arred_liq
        sign     = 'X'
      IMPORTING
        output   = vl_arred_liq.

    vl_arred_liq = vl_arred_liq * wa_zsdt0041_aux-zmeng.

    CALL FUNCTION 'ROUND'
      EXPORTING
        decimals = 2
        input    = vl_arred_liq
        sign     = 'X'
      IMPORTING
        output   = vl_arred_liq.

* Arredondando parte da taxa do imposto do ajuste

    vl_arred_tx = wa_zsdt0041_aux-vlr_icms.

*    CALL FUNCTION 'ROUND'
*      EXPORTING
*        DECIMALS = 4
*        INPUT    = VL_ARRED_TX
*        SIGN     = 'X'
*      IMPORTING
*        OUTPUT   = VL_ARRED_TX.

    vl_arred_tx = vl_arred_tx * wa_zsdt0041_aux-zmeng.

    CALL FUNCTION 'ROUND'
      EXPORTING
        decimals = 2
        input    = vl_arred_tx
        sign     = 'X'
      IMPORTING
        output   = vl_arred_tx.

* Cálculo do ajuste

    wa_zsdt0041_ajuste-desc_absoluto = wa_zsdt0041_aux-zwert * wa_zsdt0041_aux-zmeng - ( vl_arred_tx + vl_arred_liq ).
    ADD vl_kbetr TO wa_zsdt0041_ajuste-desc_absoluto.

    APPEND wa_zsdt0041_ajuste TO it_zsdt0041_ajuste.

    "Chamar a função e passar a 0041 global após gerar ordem

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'S0300'.
  SET TITLEBAR 'T0300'.

  IF nao EQ abap_true.
    LOOP AT SCREEN.
      IF screen-name EQ 'VLR'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE sy-ucomm.
    WHEN 'OK'.

      IF sim EQ abap_true.
        PERFORM imprime_contrato USING wg_header-doc_simulacao.
        PERFORM imprime_ov.
      ELSE.
        PERFORM imprime_contrato USING wg_header-doc_simulacao.
      ENDIF.

      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  CHECK_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_campos .
  IF nao EQ abap_true.
    LOOP AT SCREEN.
      IF screen-name EQ 'VLR'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  PBO0108  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo0108 OUTPUT.
  SET PF-STATUS 'PF0108'.
  SET TITLEBAR  'TI0108'.
  "
  "ALV
  IF container_3 IS INITIAL.
*    WA_LAYOUT-STYLEFNAME = 'STYLE'.
*    WA_LAYOUT-ZEBRA      = C_X.
*    WA_LAYOUT-NO_ROWMARK = C_X.
*    WA_STABLE-ROW        = C_X.
*    WA_LAYOUT-NO_TOOLBAR = ''.
*    WA_LAYOUT-INFO_FNAME = 'LINE_COLOR'.
*    WA_LAYOUT-SEL_MODE   = 'A'.
*    WA_LAYOUT-BOX_FNAME  = 'MARK'.

*    WA_STABLE-ROW = 'X'.
**    WA_STABLE-COL = 'X'.

    wa_layout-cwidth_opt = c_x.
    wa_layout-zebra       = c_x.
    wa_layout-no_rowmark  = space.
    wa_layout-col_opt     = c_x.
*    WA_STABLE-ROW         = C_X.
    wa_layout-sel_mode    = 'A'.
    wa_layout-info_fname  = 'LINE_COLOR'.
    wa_layout-stylefname = 'STYLE'.

    CREATE OBJECT container_3
      EXPORTING
        container_name = 'CC_ANTEC'.

    CREATE OBJECT grid3
      EXPORTING
        i_parent = container_3.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid3.

    REFRESH tl_function.

    SET HANDLER: obg_toolbar->on_toolbar FOR grid3,
                 obg_toolbar->handle_user_command FOR grid3.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    PERFORM montar_layout_antec.


    CALL METHOD grid3->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_antec[].

    CALL METHOD grid3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler=>on_hotspot_click         FOR grid3.
  ELSE.
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI0108  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai0108 INPUT.
  CASE sy-ucomm.
    WHEN 'SAIR'.
      LEAVE TO SCREEN 0.
    WHEN 'CTB'.
      PERFORM gerar_contabil.
    WHEN 'ELIM'.
      PERFORM eliminar.
    WHEN 'EST'.
      PERFORM estornar_contabil.
    WHEN 'REFRESH'.
      PERFORM carrega_antec USING tg_antec[] wg_header-doc_simulacao.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ANTEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_antec.

  REFRESH t_fieldcatalog.

  PERFORM montar_estrutura USING:
        1   ' '        ' '             'TG_ANTEC'  'VBELN'        'Nº OV'               '10'   ' ' ' '   ' '   ' '  ' '  ' ',
        2   ' '        ' '             'TG_ANTEC'  'ZID_LANC'     'ID Lanç'             '10'   ' ' ' '   ' '   ' '  ' '  ' ',
        3   'ZSDT0159' 'MONT_MOEDA'    'TG_ANTEC'  'MONT_MOEDA'   'Valor Total'         '15'   ' ' ' '   ' '   ' '  ' '  ' ',
        4   ' '        ' '             'TG_ANTEC'  'ADIANT'       'Doc. Contábil'       '10'   ' ' ' '   ' '   ' '  ' '  'X',
        5   ' '        ' '             'TG_ANTEC'  'AUGBL'        'Doc. Compensação'    '10'   ' ' ' '   ' '   ' '  ' '  'X',
        6   ' '        ' '             'TG_ANTEC'  'USNAM'        'Usuário'             '12'   ' ' ' '   ' '   ' '  ' '  ' ',
        7   'ZSDT0159' 'DATA_ATUAL'    'TG_ANTEC'  'DATA_ATUAL'   'Data'                '12'   ' ' ' '   ' '   ' '  ' '  ' ',
        8   ' '        ' '             'TG_ANTEC'  'USNAM_E'      'Usuário Est.'        '12'   ' ' ' '   ' '   ' '  ' '  ' ',
        9   'ZSDT0159' 'DATA_ATUAL_E'  'TG_ANTEC'  'DATA_ATUAL_E' 'Data Est.'           '12'   ' ' ' '   ' '   ' '  ' '  ' ',

        " 21.02.2023 - RAMON - 102323 -->
        10 'ZSDT0159' 'ID_TRANSACAO_FINANCEIRA' 'TG_ANTEC' 'ID_TRANSACAO_FINANCEIRA' 'ID Transação Financeira ' '12'   ' ' ' '   ' '   ' '  ' '  ' '.
  " 21.02.2023 - RAMON - 102323 --<

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CARREGA_MEMO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_antec USING tg_antec TYPE tg_antec_f
                         doc_simulacao TYPE zsdt0040-doc_simulacao.
  DATA: it_zib_contabil_chv TYPE TABLE OF zib_contabil_chv,
        it_bsad             TYPE TABLE OF bsad_view,
        tabix               TYPE sy-tabix.

  REFRESH tg_antec.
  SELECT *
    FROM zsdt0159
    INTO CORRESPONDING FIELDS OF TABLE tg_antec
  WHERE doc_simulacao = doc_simulacao.

  CHECK tg_antec[] IS NOT INITIAL.

*  SELECT *
*    FROM ZIB_CONTABIL_CHV
*    INTO TABLE IT_ZIB_CONTABIL_CHV
*   FOR ALL ENTRIES IN TG_ANTEC
*    WHERE OBJ_KEY EQ TG_ANTEC-OBJ_KEY.
*
*  IF IT_ZIB_CONTABIL_CHV[] IS NOT INITIAL.
*    SELECT *
*      FROM BSAD
*      INTO TABLE IT_BSAD
*      FOR ALL ENTRIES IN IT_ZIB_CONTABIL_CHV
*      WHERE BUKRS EQ IT_ZIB_CONTABIL_CHV-BUKRS
*      AND   BELNR EQ IT_ZIB_CONTABIL_CHV-BELNR
*      AND   GJAHR EQ IT_ZIB_CONTABIL_CHV-GJAHR
*      AND   BSAD~BELNR NE BSAD~AUGBL.
*
*  ENDIF.

  SELECT *
    FROM bsad_view
    INTO TABLE @it_bsad
    FOR ALL ENTRIES IN @tg_antec
    WHERE bukrs EQ @tg_antec-bukrs
    AND   belnr EQ @tg_antec-adiant
    AND   gjahr EQ @tg_antec-gjahr
    AND   bsad_view~belnr NE bsad_view~augbl.

  SORT: it_zib_contabil_chv BY obj_key,
        it_bsad BY bukrs belnr gjahr.

  LOOP AT tg_antec INTO DATA(wg_antec).
    tabix = sy-tabix.
    IF wg_antec-adiant IS NOT INITIAL.
      SELECT SINGLE   *
         FROM bkpf
         INTO @DATA(wg_bkpf_fb082)
         WHERE bukrs EQ @wg_antec-bukrs
         AND   belnr EQ @wg_antec-adiant
         AND   gjahr EQ @wg_antec-gjahr
         AND   stblg NE ''.
      IF sy-subrc = 0.
        wg_antec-estorno = 'X'.
      ENDIF.
      READ TABLE it_bsad INTO DATA(wl_bsad2) WITH KEY bukrs = wg_antec-bukrs
                                                     belnr = wg_antec-adiant
                                                     gjahr = wg_antec-gjahr BINARY SEARCH.
      IF sy-subrc = 0.
        SELECT SINGLE   *
           FROM bkpf
           INTO @DATA(wg_bkpf082)
           WHERE bukrs EQ @wg_antec-bukrs
           AND   belnr EQ @wl_bsad2-augbl
           AND   gjahr EQ @wl_bsad2-augdt(4)
           AND   stblg NE ''.
        IF sy-subrc NE 0.
          wg_antec-augbl = wl_bsad2-augbl.
          wg_antec-augdt = wl_bsad2-augdt.
        ENDIF.
      ENDIF.

    ELSE.

      READ TABLE it_zib_contabil_chv INTO DATA(wl_zib) WITH KEY obj_key = wg_antec-obj_key BINARY SEARCH.
      IF sy-subrc = 0.
        wg_antec-bukrs  = wl_zib-bukrs.
        wg_antec-adiant = wl_zib-belnr.
        wg_antec-gjahr  = wl_zib-gjahr.
        SELECT SINGLE   *
             FROM bkpf
             INTO @DATA(wg_bkpf_fb08)
             WHERE bukrs EQ @wl_zib-bukrs
             AND   belnr EQ @wl_zib-belnr
             AND   gjahr EQ @wl_zib-gjahr
             AND   stblg NE ''.
        IF sy-subrc = 0.
          wg_antec-estorno = 'X'.
        ENDIF.
        READ TABLE it_bsad INTO DATA(wl_bsad) WITH KEY bukrs = wl_zib-bukrs
                                                       belnr = wl_zib-belnr
                                                       gjahr = wl_zib-gjahr BINARY SEARCH.
        IF sy-subrc = 0.
          SELECT SINGLE   *
             FROM bkpf
             INTO @DATA(wg_bkpf08)
             WHERE bukrs EQ @wl_zib-bukrs
             AND   belnr EQ @wl_bsad-augbl
             AND   gjahr EQ @wl_bsad-augdt(4)
             AND   stblg NE ''.
          IF sy-subrc NE 0.
            wg_antec-augbl = wl_bsad-augbl.
            wg_antec-augdt = wl_bsad-augdt.
          ENDIF.
        ENDIF.
      ELSE.
        SELECT SINGLE *
          FROM zib_contabil
          INTO @DATA(wl_zibc)
         WHERE obj_key = @wg_antec-obj_key.
        IF sy-subrc = 0.
          wg_antec-adiant = icon_operation.
        ENDIF.
        SELECT SINGLE *
          FROM zib_contabil_err
          INTO @DATA(wl_zibe)
         WHERE obj_key = @wg_antec-obj_key.
        IF sy-subrc = 0.
          wg_antec-adiant = icon_message_error_small.
        ENDIF.
      ENDIF.
    ENDIF.
    IF wg_antec-estorno = 'X'.
      wg_antec-line_color = 'C601'.
    ENDIF.
    MODIFY tg_antec FROM wg_antec INDEX tabix TRANSPORTING bukrs adiant gjahr augbl augdt line_color estorno.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GERAR_CONTABIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gerar_contabil .
  DATA: it_zsdt0041     TYPE TABLE OF zsdt0041,
        it_zsdt0041r    TYPE TABLE OF zsdt0041,
        it_zsdt0159     TYPE TABLE OF zsdt0159,
        it_zfit0026     TYPE TABLE OF zfit0026,
        it_zib_contabil TYPE TABLE OF zib_contabil.

  DATA: wa_zsdt0159     TYPE zsdt0159,
        wa_zib          TYPE zib_contabil,
        wa_zfit0026     TYPE zfit0026,
        wa_zib_contabil TYPE zib_contabil,
        w_zsdt0090      TYPE zsdt0090.

  DATA: vseq         TYPE zsdt0159-seq,
        vseq2        TYPE zfit0026-seq,
        vseqc(6),
        vgrava(1)    VALUE ' ',
        vcheck(1),
        wl_data(10),
        wl_venc(10),
        wl_bktxt(25),
        wl_valor(16),
        vl_msg_aux   TYPE string,
        opt          TYPE ctu_params,
        p_zid        TYPE numc10,
        vl_netwr     TYPE netwr,
        vl_mwsbp     TYPE mwsbp.

  SORT tg_antec BY vbeln estorno.
  "
  SELECT *
   FROM zsdt0041
   INTO TABLE it_zsdt0041
   WHERE doc_simulacao EQ wg_header-doc_simulacao
   AND   vbeln         NE ''.

  IF it_zsdt0041[] IS INITIAL.
    MESSAGE 'Não há ordens de venda criadas!' TYPE 'I'.
    EXIT.
  ENDIF.

*** Projeto E-commerce #102323
  IF wg_header-ecommerce EQ 'X'.
    IF zsdt0159-id_transacao_financeira IS INITIAL.
      MESSAGE 'Obrigatório informar o ID Transação Financeira!' TYPE 'I'.
      EXIT.
    ENDIF.
  ENDIF.
*** Projeto E-commerce #102323

*** BUG - 83481 - Inicio - CBRAND
  SELECT SINGLE *
   FROM zsdt0040
  INTO  @DATA(w_zsdt0040)
   WHERE doc_simulacao EQ @wg_header-doc_simulacao.

*** BUG - 83481 - Inicio - CBRAND

  it_zsdt0041r[] = it_zsdt0041[].
  SORT it_zsdt0041r BY vbeln.
  DELETE ADJACENT DUPLICATES FROM it_zsdt0041r COMPARING vbeln.

  LOOP AT it_zsdt0041r INTO DATA(wl_41r).
    CLEAR:  wa_zfit0026, tg_antec, wa_zsdt0159.
    READ TABLE tg_antec WITH KEY vbeln    = wl_41r-vbeln
                                 estorno  = ' ' BINARY SEARCH.
    IF sy-subrc NE 0 OR tg_antec-adiant EQ icon_message_error_small.
      vgrava = 'X'.
      CLEAR: vseq, vseq2.
      "
      CLEAR wl_41r-vlrtot.
      LOOP AT it_zsdt0041 INTO DATA(wl_41) WHERE vbeln = wl_41r-vbeln.
        ADD wl_41-vlrtot TO wl_41r-vlrtot.
      ENDLOOP.
      IF tg_antec-adiant EQ icon_message_error_small.
        wa_zfit0026-obj_key = tg_antec-obj_key.
        DELETE FROM zib_contabil WHERE obj_key = tg_antec-obj_key.
        DELETE FROM zib_contabil_err WHERE obj_key = tg_antec-obj_key.
        COMMIT WORK.
      ELSE.
        "sequencia 1
        SELECT MAX( seq )
          INTO vseq
          FROM zsdt0159
        WHERE doc_simulacao = wl_41r-doc_simulacao
        AND   vbeln         = wl_41r-vbeln.
        ADD 1 TO vseq.

        "Sequencia 2
        SELECT COUNT(*)
            INTO vseq2
            FROM zfit0026
        WHERE vbeln EQ wl_41r-vbeln.
        ADD 1 TO vseq2.

        vseqc = vseq2.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = vseqc
          IMPORTING
            output = vseqc.

        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr = '01'
            object      = 'ZID_LANC'
          IMPORTING
            number      = p_zid.
        CONCATENATE wl_41r-vbeln vseqc sy-datum(4) INTO wa_zfit0026-obj_key.

        vcheck = 'X'.
        WHILE vcheck EQ 'X'.

          SELECT SINGLE * FROM zib_contabil
            INTO wa_zib
            WHERE obj_key EQ wa_zfit0026-obj_key.

          IF sy-subrc IS INITIAL.

            ADD 1 TO vseqc.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = vseqc
              IMPORTING
                output = vseqc.

            CONCATENATE wl_41r-vbeln vseqc sy-datum(4) INTO wa_zsdt0159-obj_key.

          ELSE.
            vcheck = ' '.
          ENDIF.
        ENDWHILE.

      ENDIF.
******************************************************************      "Comentado Aoenning "21/02/2020
      "WG_HEADER-TPSIM
*      WA_ZFIT0026-ZID_LANC       = P_ZID.
*      WA_ZFIT0026-DOCNUM         = ''.
*      WA_ZFIT0026-VBELN          = WL_41R-VBELN.
*      WA_ZFIT0026-SEQ            = VSEQ2.
*      WA_ZFIT0026-DATA_VENC      = SWITCH #( WG_HEADER-TPSIM WHEN 'VV' THEN WG_HEADER-DTVENCOV
*                                                             WHEN 'VP' THEN WG_HEADER-DTPGTCULT
*                                                             ELSE '' ).
*      WA_ZFIT0026-MOEDA          = WG_HEADER-WAERK.
*      WA_ZFIT0026-MONT_MOEDA     = WL_41R-VLRTOT.
*      WA_ZFIT0026-TAXA           = 1.
*      WA_ZFIT0026-MONT_MI        = WL_41R-VLRTOT.
*      WA_ZFIT0026-FORMA_PAG      = 'D'.
*      WA_ZFIT0026-STATUS         = 'G'.
*      WA_ZFIT0026-UNAME          = SY-UNAME.
*      WA_ZFIT0026-DATA_REGISTRO  = SY-DATUM.
*      WA_ZFIT0026-BUKRS          = WG_HEADER-VKORG.
*      WA_ZFIT0026-RAZAO_ESPECIAL = ''.
*      WA_ZFIT0026-ELIMINADO      = ''.
*      WA_ZFIT0026-OBSERVACAO     = 'GERACAO DE BOLETO BANCARIO AUTOMÁTICA'.
*      WA_ZFIT0026-ZTERM          = 'I003'.
************************************************************************************************************
      SELECT *
        FROM zsdt0090
        INTO TABLE @DATA(t_zsdt0090)
        WHERE doc_simulacao  EQ @wl_41r-doc_simulacao
        AND   vbelv          EQ @wl_41r-vbeln
        AND   categoria      EQ 'V'
        AND   estorno        NE 'X'
        ORDER BY sequencia DESCENDING.

      IF t_zsdt0090[] IS NOT INITIAL.
        READ TABLE t_zsdt0090 INTO w_zsdt0090 INDEX 1.
        wa_zsdt0159-data_venc      = w_zsdt0090-valdt.
      ELSE.
        wa_zsdt0159-data_venc = SWITCH #( wg_header-tpsim WHEN 'VV' THEN wg_header-dtvencov
                                                          WHEN 'VP' THEN wg_header-dtpgtcult
                                                          ELSE '' ).

      ENDIF.

      IF tg_antec-adiant NE icon_message_error_small.
*        APPEND WA_ZFIT0026 TO IT_ZFIT0026.   "Comentado Aoenning "21/02/2020
      ENDIF.

      " Grava na ZIB
      SELECT SINGLE *
        FROM vbak
        INTO @DATA(wl_vbak)
        WHERE vbeln = @wl_41r-vbeln.

      "E-commerce - ZSDT0044 - Ajustar Gerar Pag. Antencip #112462 BG
      CLEAR: vl_netwr, vl_mwsbp.
      SELECT SUM( netwr ) SUM( mwsbp )
      FROM vbap
      INTO (vl_netwr, vl_mwsbp)
      WHERE vbeln = wl_41r-vbeln.




      wa_zsdt0159-doc_simulacao   = wl_41r-doc_simulacao.
      wa_zsdt0159-vbeln           = wl_41r-vbeln.
      wa_zsdt0159-seq             = vseq.
      wa_zsdt0159-zid_lanc        = p_zid. "WA_ZFIT0026-ZID_LANC. "Comentado Aoenning "21/02/2020
*      WA_ZSDT0159-OBJ_KEY         = WA_ZFIT0026-OBJ_KEY.  "Comentado Aoenning "21/02/2020
      wa_zsdt0159-bukrs           = wl_vbak-vkorg.
      wa_zsdt0159-gjahr           = sy-datum+0(4).
      wa_zsdt0159-mont_moeda      = vl_netwr + vl_mwsbp. "wl_41r-vlrtot. "E-commerce - ZSDT0044 - Ajustar Gerar Pag. Antencip #112462 BG
*      WA_ZSDT0159-DATA_VENC       = WA_ZFIT0026-DATA_VENC. "Comentado Aoenning "21/02/2020
      wa_zsdt0159-usnam           = sy-uname.
      wa_zsdt0159-data_atual      = sy-datum.
      wa_zsdt0159-hora_atual      = sy-uzeit.

      " 21.02.2023 - RAMON - 102323 -->
      wa_zsdt0159-id_transacao_financeira = zsdt0159-id_transacao_financeira.

      " 21.02.2023 - RAMON - 102323 --<


      IF tg_antec-adiant NE icon_message_error_small.
        APPEND wa_zsdt0159 TO it_zsdt0159.
      ENDIF.
      "
      WRITE sy-datum TO wl_data.
      WRITE wa_zsdt0159-data_venc TO wl_venc.
*      WRITE WA_ZFIT0026-MONT_MOEDA TO WL_VALOR."WL_41R-VLRTOT.
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
*                 WRITE wl_41r-vlrtot TO wl_valor."WL_41R-VLRTOT.
      WRITE wa_zsdt0159-mont_moeda TO wl_valor."WL_41R-VLRTOT.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

      CONCATENATE 'VENDA' wl_41r-doc_simulacao INTO wl_bktxt SEPARATED BY space.

      " 21.02.2023 - RAMON - 102323 -->

      DATA lv_pe_ecomm TYPE dzuonr.

      IF wg_header-ecommerce = 'X'.

        lv_pe_ecomm = 'PE' && wg_header-id_order_ecommerce.

      ENDIF.
      " 21.02.2023 - RAMON - 102323 --<

      REFRESH: tg_msg, tg_bdc.
      PERFORM f_preencher_dynpro USING:
                'X' 'SAPMF05A'                      '0113',
                ' ' 'BKPF-BLDAT'                    wl_data,
                ' ' 'BKPF-BLART'                    'DZ',
                ' ' 'BKPF-BUKRS'                    wl_vbak-vkorg,
                ' ' 'BKPF-BUDAT'                    wl_data,
                ' ' 'BKPF-MONAT'                    wl_data+3(2),
                ' ' 'BKPF-WAERS'                    wl_vbak-waerk,
                ' ' 'BKPF-BKTXT'                    wl_bktxt,
                ' ' 'BKPF-XBLNR'                    wl_bktxt,
                ' ' 'RF05A-NEWKO'                   wl_vbak-kunnr,
*                ' ' 'RF05A-ZUMSK'                   'A',
                ' ' 'RF05A-ZUMSK'                   'L',
                ' ' 'BDC_OKCODE'                    '/00'.


      PERFORM f_preencher_dynpro USING:
             'X' 'SAPMF05A'                      '0304',
             ' ' 'BSEG-WRBTR'                    wl_valor,
             ' ' 'BSEG-GSBER'                    wl_41r-werks,
             ' ' 'BSEG-ZFBDT'                    wl_venc,

             " 21.02.2023 - RAMON - 106134 -->
             ' ' 'BSEG-ZLSCH'                    w_zsdt0040-meio_pago,
             "' ' 'BSEG-ZLSCH'                    'D',
             " 21.02.2023 - RAMON - 106134 --<

             " 21.02.2023 - RAMON - 102323 -->
             ' ' 'BSEG-ZUONR'                    lv_pe_ecomm,
             " 21.02.2023 - RAMON - 102323 --<
             ' ' 'BDC_OKCODE'                    '=ZK'.

      PERFORM f_preencher_dynpro USING:
             'X' 'SAPMF05A'                      '0331',
             ' ' 'BSEG-HBKID'                    w_zsdt0040-hbkid, "'BBRA', BUG - 83481 - CBRAND
             ' ' 'BSEG-HZUON'                    wl_41r-vbeln,
             ' ' 'BDC_OKCODE'                    '=BU'.

      opt-dismode = 'N'.
      opt-defsize = 'X'.
      CALL TRANSACTION 'F-37' USING tg_bdc
        OPTIONS FROM opt
        MESSAGES INTO tg_msg.


      READ TABLE tg_msg
        WITH KEY msgtyp = 'S'
                 msgnr  = '312'.
      IF  sy-subrc IS INITIAL.
        CONDENSE tg_msg-msgv1 NO-GAPS.
        READ TABLE it_zsdt0159 INTO wa_zsdt0159 WITH KEY doc_simulacao   = wl_41r-doc_simulacao
                                                         vbeln           = wl_41r-vbeln
                                                         seq             = vseq.
        wa_zsdt0159-adiant = tg_msg-msgv1.
        wa_zsdt0159-gjahr  = sy-datum+0(4).
        MODIFY it_zsdt0159 FROM wa_zsdt0159 INDEX sy-tabix TRANSPORTING adiant gjahr.
      ELSE.
        LOOP AT tg_msg WHERE msgtyp = 'E'.
          MESSAGE ID tg_msg-msgid TYPE 'S' NUMBER tg_msg-msgnr WITH tg_msg-msgv1 tg_msg-msgv2 tg_msg-msgv3 tg_msg-msgv4 INTO vl_msg_aux.
          MESSAGE vl_msg_aux TYPE 'I'.
        ENDLOOP.
      ENDIF.
*                              USING TL_PGT_ANT.

*      " Partida 1 e 2
*      DO 2 TIMES.
*        WA_ZIB_CONTABIL-OBJ_KEY     = WA_ZFIT0026-OBJ_KEY.
*        WA_ZIB_CONTABIL-SEQITEM     = SY-INDEX.
*        WA_ZIB_CONTABIL-GSBER       = WL_41R-WERKS.
*        WA_ZIB_CONTABIL-BUKRS       = WL_VBAK-VKORG.
*        WA_ZIB_CONTABIL-INTERFACE   = '96'.
*        WA_ZIB_CONTABIL-BKTXT       = 'Recbto Venda Insumos'.
*        CONCATENATE SY-DATUM+6(2) '.' SY-DATUM+4(2) '.' SY-DATUM+0(4) INTO WA_ZIB_CONTABIL-BLDAT.
*        WA_ZIB_CONTABIL-BUDAT       = WA_ZIB_CONTABIL-BLDAT.
*        WA_ZIB_CONTABIL-GJAHR       = SY-DATUM+0(4).
*        WA_ZIB_CONTABIL-MONAT       = SY-DATUM+4(2).
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            INPUT  = WL_41R-VBELN
*          IMPORTING
*            OUTPUT = WA_ZIB_CONTABIL-XBLNR.
*
*        WA_ZIB_CONTABIL-BLART       = 'NC'.
*
*        " Lançamento - Conta de Débito
*        CONCATENATE 'Ordem de Venda Insumos' WL_41R-VBELN INTO WA_ZIB_CONTABIL-SGTXT SEPARATED BY SPACE.
*
*        IF SY-INDEX = 1.
*          WA_ZIB_CONTABIL-BSCHL          = '09'.
*        ELSE.
*          WA_ZIB_CONTABIL-BSCHL          = '19'.
*        ENDIF.
*        WA_ZIB_CONTABIL-HKONT          = WL_VBAK-KUNNR.
*        WA_ZIB_CONTABIL-WAERS          = WL_VBAK-WAERK.
*        CONCATENATE WA_ZFIT0026-DATA_VENC+6(2) '.' WA_ZFIT0026-DATA_VENC+4(2) '.' WA_ZFIT0026-DATA_VENC+0(4) INTO  WA_ZIB_CONTABIL-ZFBDT.
*        WA_ZIB_CONTABIL-ZLSPR          = 'A'.
*        WA_ZIB_CONTABIL-ZLSCH          = 'U'.
*        WA_ZIB_CONTABIL-KIDNO          = SPACE.
*        WA_ZIB_CONTABIL-XREF1          = SPACE.
*        WA_ZIB_CONTABIL-XREF2          = SPACE.
*        WA_ZIB_CONTABIL-XREF3          = SPACE.
*        WA_ZIB_CONTABIL-BUPLA          = WL_VBAK-VKBUR.
*        WA_ZIB_CONTABIL-ZTERM          = WA_ZFIT0026-ZTERM.
*
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            INPUT  = WL_41R-VBELN
*          IMPORTING
*            OUTPUT = WA_ZIB_CONTABIL-ZUONR.
*
*
*        WA_ZIB_CONTABIL-UMSKZ  = 'L'.
*        WA_ZIB_CONTABIL-KOSTL          = SPACE.
*        WA_ZIB_CONTABIL-AUFNR          = SPACE.
*        WA_ZIB_CONTABIL-PRCTR          = SPACE.
*        WA_ZIB_CONTABIL-WAERS_I        = 'BRL'.
*        WA_ZIB_CONTABIL-WAERS_F        = WL_VBAK-WAERK.
*
*        IF ( WL_VBAK-WAERK EQ 'USD' ).
*          WA_ZIB_CONTABIL-WRBTR          = WA_ZFIT0026-MONT_MOEDA.
*          WA_ZIB_CONTABIL-DMBTR          = WA_ZFIT0026-MONT_MOEDA.
*          WA_ZIB_CONTABIL-DMBE2          = 0.
*        ELSE.
*          WA_ZIB_CONTABIL-WRBTR          = WA_ZFIT0026-MONT_MOEDA.
*          WA_ZIB_CONTABIL-DMBTR          = WA_ZFIT0026-MONT_MOEDA.
*          WA_ZIB_CONTABIL-DMBE2          = 0.
*        ENDIF.
*
*        WA_ZIB_CONTABIL-BVTYP          = SPACE.
*        WA_ZIB_CONTABIL-HBKID          = 'BBRA'.
*        WA_ZIB_CONTABIL-RG_ATUALIZADO  = 'N'.
*        WA_ZIB_CONTABIL-BANKL          = SPACE.
*        WA_ZIB_CONTABIL-BANKN          = SPACE.
*        WA_ZIB_CONTABIL-NEWBW          = SPACE.
*        WA_ZIB_CONTABIL-ANLN1          = SPACE.
*        WA_ZIB_CONTABIL-ANLN2          = SPACE.
*        APPEND WA_ZIB_CONTABIL TO IT_ZIB_CONTABIL.
*      ENDDO.
*      "
*      "fim ZIB
*      "
    ENDIF.
  ENDLOOP.
  CLEAR zsdt0159-id_transacao_financeira.
  "
*  CHECK VGRAVA = 'X'.

*  MODIFY ZFIT0026     FROM TABLE IT_ZFIT0026.
*  MODIFY ZIB_CONTABIL FROM TABLE IT_ZIB_CONTABIL.

  DELETE it_zsdt0159 WHERE adiant IS INITIAL.
  IF it_zsdt0159[] IS NOT INITIAL.
*    MODIFY ZFIT0026     FROM TABLE IT_ZFIT0026.
    MODIFY zsdt0159     FROM TABLE it_zsdt0159.
    COMMIT WORK.
  ENDIF.

  PERFORM carrega_antec USING tg_antec[] wg_header-doc_simulacao.

ENDFORM.

FORM eliminar.
  DATA w_erro(1) VALUE ''.

  CALL METHOD grid3->get_selected_rows
    IMPORTING
      et_index_rows = tl_index_rows.

  LOOP AT tl_index_rows INTO wl_index_rows.
    READ TABLE tg_antec INDEX  wl_index_rows-index.
    IF sy-subrc = 0.
      IF tg_antec-adiant EQ icon_message_error_small
         OR tg_antec-estorno IS NOT INITIAL.
        DELETE FROM zsdt0159 WHERE zid_lanc = tg_antec-zid_lanc.
        DELETE FROM zfit0026 WHERE zid_lanc = tg_antec-zid_lanc.
        IF tg_antec-adiant EQ icon_message_error_small .
          DELETE FROM zib_contabil     WHERE obj_key = tg_antec-obj_key.
          DELETE FROM zib_contabil_err WHERE obj_key = tg_antec-obj_key.
        ENDIF.
        COMMIT WORK.
      ELSE.
        w_erro = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.
  "
  IF w_erro = 'X'.
    MESSAGE 'Itens selecionados não podem ser eliminados!' TYPE 'I'.
  ENDIF.
  PERFORM carrega_antec USING tg_antec[] wg_header-doc_simulacao.
  "
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ESTORNAR_CONTABIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM estornar_contabil .
  DATA: vest(1)      VALUE '',
        vdata(10),
        p_erro(1),
        zib_data_doc TYPE bkpf-bldat,
        wa_zib       TYPE zib_contabil.

  LOOP AT tg_antec.
    IF tg_antec-adiant IS NOT INITIAL AND
      tg_antec-adiant NE icon_message_error_small AND
      tg_antec-adiant NE icon_operation AND
      tg_antec-augbl IS INITIAL AND
      tg_antec-estorno IS INITIAL.
      vest = 'X'.
    ENDIF.
  ENDLOOP.
  "
  IF vest IS INITIAL.
    MESSAGE 'Não há documentos para estornar!' TYPE 'I'.
    EXIT.
  ENDIF.
  "
  CALL METHOD grid3->get_selected_rows
    IMPORTING
      et_index_rows = tl_index_rows.

  LOOP AT tl_index_rows INTO wl_index_rows.
    READ TABLE tg_antec INDEX  wl_index_rows-index.
    CHECK sy-subrc = 0.
    IF tg_antec-adiant IS NOT INITIAL
       AND tg_antec-adiant NE icon_message_error_small
       AND tg_antec-adiant NE icon_operation
       AND tg_antec-augbl IS INITIAL
       AND tg_antec-estorno IS INITIAL.

*      SELECT SINGLE * FROM ZIB_CONTABIL INTO WA_ZIB WHERE OBJ_KEY EQ TG_ANTEC-OBJ_KEY.
*
*      IF SY-SUBRC IS INITIAL.
      SELECT SINGLE bldat
        FROM bkpf
        INTO zib_data_doc
        WHERE bukrs = tg_antec-bukrs
        AND   belnr = tg_antec-adiant
        AND   gjahr = tg_antec-gjahr.

      FREE: ti_bdcdata.
      CLEAR: zib_data_doc, vdata.

*      CONCATENATE WA_ZIB-BLDAT+6(4) WA_ZIB-BLDAT+3(2) INTO ZIB_DATA_DOC.

      IF zib_data_doc(6) EQ sy-datum(6).
        "mesmo mes
        PERFORM batch_input USING:  'X' 'SAPMF05A'     '0105',
                                   ''  'BDC_CURSOR'   'UF05A-STGRD',
                                   ''  'BDC_OKCODE'   '=BU',
                                   ''  'RF05A-BELNS'  tg_antec-adiant,
                                   ''  'BKPF-BUKRS'   tg_antec-bukrs,
                                   ''  'RF05A-GJAHS'  tg_antec-gjahr,
                                   ''  'UF05A-STGRD'  '01'.
      ELSE.
        "mes retroativo
        CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum(4) INTO vdata.

        PERFORM batch_input USING:  'X' 'SAPMF05A'     '0105',
                                   ''  'BDC_CURSOR'   'UF05A-STGRD',
                                   ''  'BDC_OKCODE'   '=BU',
                                   ''  'RF05A-BELNS'  tg_antec-adiant,
                                   ''  'BKPF-BUKRS'   tg_antec-bukrs,
                                   ''  'RF05A-GJAHS'  tg_antec-gjahr,
                                   ''  'UF05A-STGRD'  '02',
                                   ''  'BSIS-BUDAT'   vdata,
                                   ''  'BSIS-MONAT'   sy-datum+4(2).
      ENDIF.
*      ENDIF.

      CLEAR p_erro.
      PERFORM zf_call_transaction TABLES it_msg USING 'FB08' CHANGING p_erro wg_documento.
      IF p_erro IS INITIAL.
        UPDATE zsdt0159 SET estorno       = 'X'
                            usnam_e       = sy-uname
                            data_atual_e  = sy-datum
                            hora_atual_e  = sy-uzeit
        WHERE  obj_key =  tg_antec-obj_key.

        DELETE FROM zfit0026 WHERE zid_lanc = tg_antec-zid_lanc.

        COMMIT WORK.
        WAIT UP TO 2 SECONDS.
      ELSE.
        READ TABLE it_msg INTO DATA(wa_mess) WITH KEY msgtyp = 'E'.
        IF sy-subrc IS INITIAL.
          MESSAGE ID wa_mess-msgid TYPE 'S' NUMBER wa_mess-msgnr WITH wa_mess-msgv1 wa_mess-msgv2 wa_mess-msgv3 wa_mess-msgv4
          DISPLAY LIKE wa_mess-msgtyp.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM carrega_antec USING tg_antec[] wg_header-doc_simulacao.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LOG_ACAO
*&---------------------------------------------------------------------*
*       Gravar as LOG de Ações Aprovar / Reprovar / Bloquear
*----------------------------------------------------------------------*
*      -->P_5342   text
*----------------------------------------------------------------------*
FORM log_acao USING p_acao
                    p_complemento
    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
           CHANGING p_cancelado.
  " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL <--

  DATA: tl_texto TYPE catsxt_longtext_itab,
        wl_texto TYPE LINE OF catsxt_longtext_itab.
  DATA motivo TYPE zsded033.
  DATA: id TYPE zsded032.

  " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
  DATA lr_tpsim TYPE RANGE OF char2.

  APPEND 'IEQAD' TO lr_tpsim.
  APPEND 'IEQVV' TO lr_tpsim.
  APPEND 'IEQVP' TO lr_tpsim.
  APPEND 'IEQTS' TO lr_tpsim.
  APPEND 'IEQTV' TO lr_tpsim.
  APPEND 'IEQVF' TO lr_tpsim.

  " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

  CLEAR: tg_log_acao.

  " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
  CLEAR lv_erro.
  " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

  PERFORM get_next_number
              USING
                 'ZID_LOG'
                 '01'
              CHANGING
                 id.

  REFRESH: tl_texto.
  CLEAR: convert_texto.

  CASE p_acao.
    WHEN 'A' OR 'R' OR 'B'.

      " 04.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->

      IF p_acao = 'A'.

        CLEAR gs_0308.

        SELECT SINGLE * FROM zsdt0308
          INTO gs_0308
            WHERE doc_simu = wg_header-doc_simulacao.

      ENDIF.

      IF p_acao = 'A' AND wg_header-waerk = 'BRL'
        AND tpsim IN lr_tpsim
        AND gs_0308 IS INITIAL.

        PERFORM f_popup_boleta CHANGING motivo p_cancelado.

      ELSE.

        IF gs_0308 IS NOT INITIAL.
          gv_taxa_neg = gs_0308-taxa_neg.
        ENDIF.

        " 04.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title = 'Descreva o Motivo'
          CHANGING
            ch_text  = tl_texto.

        " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
        IF sy-ucomm = 'CX_CANC'.
          p_cancelado = 'X'.
          EXIT.
        ENDIF.
        " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

        LOOP AT tl_texto INTO wl_texto.
          IF sy-tabix EQ 1.
            motivo = wl_texto.
          ELSE.
            motivo = |{ motivo } { wl_texto }|.
          ENDIF.
        ENDLOOP.

      ENDIF. " 04.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL

    WHEN 'G'.
      motivo  = 'Gerando Ordens de Venda'.
    WHEN 'S'.
      motivo  = 'Ajustando Quantidade de Sacas'.
    WHEN 'V'.
      motivo  = 'Agregando Desct. Absoluto lançado pelo Cockpit'.
    WHEN 'W'.
      motivo  = 'Recalculando os Valores dos Itens com o Vlr Total.'.

*Início - Sara Oikawa - 38859 - Agosto/2020
    WHEN 'J'.
      motivo  = 'Alterando Juros ao Ano'.
    WHEN 'D'.
      motivo  = 'Alterando Valores de Adiantamento'.
*Fim - Sara Oikawa - 38859 - Agosto/2020
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
    WHEN 'C'.
      motivo  = 'Alterando Cultura/Safra Aplicação'.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
    WHEN 'T'.
      CONCATENATE 'Alterando Condição de Pagamento' p_complemento INTO motivo SEPARATED BY space.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

    WHEN 'F'.

      SELECT SINGLE funrural
        INTO @DATA(_fun)
        FROM zsdt0040
        WHERE doc_simulacao EQ @wg_header-doc_simulacao.

      IF _fun IS INITIAL.
        motivo = 'Estorno da Exceção Funrural'.
      ELSE.
        motivo = 'Exceção Funrural'.
      ENDIF.


  ENDCASE.

  tg_log_acao =
  VALUE #(
            id_historico  = id
            motivo        = motivo
            doc_simulacao = wg_header-doc_simulacao
            status        = p_acao
            usnam         = sy-uname
            data_atual    = sy-datum
            hora_atual    = sy-uzeit
         ).

  MODIFY zsdt0186 FROM tg_log_acao.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SHOW_LOG_ACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_log_acao .

  DATA: wl_layout_log TYPE  slis_layout_alv.
  DATA: BEGIN OF tl_log OCCURS 0,
          id_historico  TYPE zsdt0186-id_historico,
          doc_simulacao TYPE zsdt0186-doc_simulacao,
          status(25),
          motivo        TYPE zsdt0186-motivo,
          usnam         TYPE zsdt0186-usnam,
          data_atual    TYPE zsdt0186-data_atual,
          hora_atual    TYPE zsdt0186-hora_atual,
        END OF tl_log.

  REFRESH: tl_log.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  SORT tg_log BY id_historico.
  DELETE ADJACENT DUPLICATES FROM tg_log COMPARING id_historico.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  LOOP AT tg_log.
    MOVE-CORRESPONDING: tg_log TO tl_log.

    IF tg_log-status EQ 'A'.
      tl_log-status = 'Aprovar'.
    ELSEIF tg_log-status EQ 'R'.
      tl_log-status = 'Reprovar'.
    ELSEIF tg_log-status EQ 'B'.
      tl_log-status = 'Bloquear'.
    ELSEIF tg_log-status EQ 'S'.
      tl_log-status = 'Ajustar Sacas'.
    ELSEIF tg_log-status EQ 'G'.
      tl_log-status = 'Gerar O.V.'.
    ELSEIF tg_log-status EQ 'V'.
      tl_log-status = 'Agregar Desc. Absoluto'.
    ELSEIF tg_log-status EQ 'W'.
      tl_log-status = 'Recalculo'.
* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
*    ENDIF.

    ELSEIF tg_log-status EQ 'J'.
      tl_log-status = 'Alterar Juros Ao Ano'.
    ELSEIF tg_log-status EQ 'D'.
      tl_log-status = 'Alterar Adiantamento'.
    ELSEIF tg_log-status EQ 'C'.
      tl_log-status = 'Alterar Cultura/Safra'.
    ELSEIF tg_log-status EQ 'F'.
      tl_log-status = 'Alterar Funrural'.
    ELSEIF tg_log-status EQ 'T'.
      tl_log-status = 'Alterar Cond.Pgto'.
    ENDIF.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
    APPEND tl_log.
  ENDLOOP.

  SORT tl_log BY id_historico.

  PERFORM montar_layout_log.

  wl_layout_log-zebra = c_x.
  wl_layout_log-colwidth_optimize = c_x.
  wl_layout_log-window_titlebar = 'Simulador de Vendas - Log de Ações'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = v_report
      is_layout             = wl_layout_log
      it_fieldcat           = estrutura[]
      i_default             = ' '
      i_save                = ' '
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 100
      i_screen_end_line     = 13
    TABLES
      t_outtab              = tl_log.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_log .

  REFRESH: estrutura.
  PERFORM montar_estrutura_alv USING:
     1  'ZSDT0186'    'ID_HISTORICO'     'TL_LOG_ACAO' 'ID_HISTORICO'  ' '            ' ' ' '  ' ' ' ',
     2  ' '           ' '                'TL_LOG_ACAO' 'STATUS'        'Ação'         ' ' ' '  ' ' ' ',
     3  'ZSDT0186'    'MOTIVO'           'TL_LOG_ACAO' 'MOTIVO'        ' '            ' ' ' '  ' ' ' ',
     4  'ZSDT0186'    'USNAM'            'TL_LOG_ACAO' 'USNAM'         'Usuário'      ' ' ' '  ' ' ' ',
     5  'ZSDT0186'    'DATA_ATUAL'       'TL_LOG_ACAO' 'DATA_ATUAL'    'Data Atual.'  ' ' ' '  ' ' ' ',
     6  'ZSDT0186'    'HORA_ATUAL'       'TL_LOG_ACAO' 'HORA_ATUAL'    'Hora Atual.'  ' ' ' '  ' ' ' '.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_4437   text
*      -->P_4438   text
*      -->P_4439   text
*      -->P_4440   text
*      -->P_4441   text
*      -->P_4442   text
*      -->P_4443   text
*      -->P_4444   text
*      -->P_4445   text
*----------------------------------------------------------------------*
FORM montar_estrutura_alv USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

  DATA: x_contador TYPE string.
  CLEAR: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-edit          = p_edit.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-reptext_ddic  = p_scrtext_l.
  wa_estrutura-outputlen     = x_contador.

  APPEND wa_estrutura TO estrutura.

ENDFORM.

*      -->P_0380   text
*      -->P_0381   text
*      -->P_0382   text
*----------------------------------------------------------------------*
FORM f_preencher_dynpro USING l_start TYPE c l_name TYPE c l_value.

  MOVE l_start TO wg_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO wg_bdc-program,
  l_value TO wg_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO wg_bdc-fnam,
      l_value TO wg_bdc-fval.
  ENDIF.
  APPEND wg_bdc TO tg_bdc.
  CLEAR: wg_bdc.

ENDFORM.                    " f_preencher_dynpro
*&---------------------------------------------------------------------*
*&      Module  CLEAR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE clear INPUT.
  CLEAR: wg_header-dtpgtcult, wg_header-meio_pago, wg_header-hbkid, wg_header-dtinijuros, wg_header-kursf, wg_header-dtvencov.
*Início - Sara Oikawa - 38859 - Agosto/2020
  CLEAR  wg_meio_pago.
*Fim - Sara Oikawa - 38859 - Agosto/2020
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  VLR_AJUSTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vlr_ajuste.

  DATA: vlr_antecipado_aux TYPE zsded005,
        vta_bruto          TYPE p DECIMALS 2,
        vlr_trotot         TYPE p DECIMALS 2,
        vlr_antecipado     TYPE  netwr_ak.

  CLEAR: vta_sistpro, vlr_trotot.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  DATA: inss       TYPE zsdt0042-vlr_perc.
  DATA: fundeinfra TYPE zsdt0042-vlr_perc.
  DATA: fethab     TYPE zsdt0042-vlr_aliq.

  IF wg_acao EQ c_atual AND sy-ucomm NE 'BTN_FUN'.
    inss   = wg_header-inss.
    fethab = wg_header-facs.

  ELSE.

    IF wg_header-tpsim EQ 'TS' OR wg_header-tpsim EQ 'TV'.

* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

      SELECT SINGLE *
      FROM kna1
      INTO @DATA(regiao)
     WHERE kunnr EQ @wg_header-kunnr.

      SELECT *
        FROM zsdt0042
        INTO TABLE @DATA(it_0042)
         WHERE cultura EQ @wg_header-cultura
           AND waerk   EQ @wg_header-waerk
           AND estado  EQ @regiao-regio
           AND safra   EQ @wg_header-safra  " RJF - 61516 - 2023.08.31
           AND val_de  LE @wg_header-dtent  " RJF - 61516 - 2023.08.31
           AND val_ate GE @wg_header-dtent. " RJF - 61516 - 2023.08.31

*   Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
*    DATA: inss TYPE zsdt0042-vlr_perc.
*    DATA: fethab TYPE zsdt0042-vlr_aliq.
*   Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

      LOOP AT it_0042 ASSIGNING FIELD-SYMBOL(<l_0042>).
        IF ( <l_0042>-witht EQ 'FR' ).
          IF regiao-stkzn IS NOT INITIAL.
            IF wg_header-funrural IS INITIAL.
              ADD <l_0042>-vlr_perc TO inss.
            ELSE.
              ADD <l_0042>-vlr_perc1 TO inss.
            ENDIF.
            ADD <l_0042>-vlr_aliq TO fethab.
          ENDIF.
        ELSEIF ( <l_0042>-witht EQ 'FI' ).
          CHECK <l_0042>-estado EQ 'GO'.
          IF wg_header-fundeinfra_exce IS INITIAL.
            ADD <l_0042>-vlr_perc TO fundeinfra.
          ELSE.
            ADD <l_0042>-vlr_perc1 TO fundeinfra.
          ENDIF.
        ELSE.
          ADD <l_0042>-vlr_perc TO inss.
          ADD <l_0042>-vlr_aliq TO fethab.
        ENDIF.
      ENDLOOP.

    ENDIF.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
    wg_header-inss = inss.
    wg_header-facs = fethab.

  ENDIF.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6


  IF wg_acao EQ c_atual AND sy-ucomm NE 'BTN_FI'.
    fundeinfra   = wg_header-fundeinfra.
  ELSE.
    IF wg_header-tpsim EQ 'TS' OR wg_header-tpsim EQ 'TV'.

      SELECT SINGLE *
      FROM kna1
      INTO regiao
     WHERE kunnr EQ wg_header-kunnr.

      SELECT *
        FROM zsdt0042
        INTO TABLE it_0042
         WHERE cultura EQ wg_header-cultura
           AND waerk   EQ wg_header-waerk
           AND estado  EQ regiao-regio
           AND safra   EQ wg_header-safra
           AND val_de  LE wg_header-dtent
           AND val_ate GE wg_header-dtent
           AND witht   EQ 'FI'.

      LOOP AT it_0042 ASSIGNING <l_0042>.
        CHECK <l_0042>-estado EQ 'GO'.
        IF wg_header-fundeinfra_exce IS INITIAL.
          ADD <l_0042>-vlr_perc TO fundeinfra.
        ELSE.
          ADD <l_0042>-vlr_perc1 TO fundeinfra.
        ENDIF.
      ENDLOOP.
    ENDIF.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
    wg_header-fundeinfra = fundeinfra.

  ENDIF.

*    TRY .
*        DATA(INSS)   = COND #( WHEN WG_HEADER-FUNRURAL IS INITIAL
*                                      THEN IT_0042[ WITHT = 'FR' ]-VLR_PERC
*                                      ELSE IT_0042[ WITHT = 'FR' ]-VLR_PERC1 ).

*        DATA(FETHAB) = IT_0042[ WITHT = 'FT' ]-VLR_ALIQ.
*      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*    ENDTRY.

  SELECT SINGLE prec_cult
     FROM zsdt0040
     INTO @DATA(vlr_prec_cult)
    WHERE doc_simulacao EQ @wg_header-doc_simulacao.

  IF ( vlr_prec_cult IS NOT INITIAL ).
    TRY.
        vlr_antecipado_aux = ( vlr_prec_cult / ( 1 + ( wg_header-antec / 100 ) ) ) .
      CATCH cx_sy_zerodivide.
    ENDTRY.
    vlr_antecipado = vlr_antecipado_aux.
  ELSE.
    vlr_antecipado = wg_header-prec_ant_cult.
  ENDIF.

  vta_bruto = convert_tratotsc * vlr_antecipado.

  SELECT SINGLE trototsc
     FROM zsdt0040
     INTO vlr_trotot
    WHERE doc_simulacao EQ wg_header-doc_simulacao.

*  VLR_TROTOT = REDUCE #( INIT X TYPE MENGE_D FOR LS IN TG_ITENS NEXT X = X + LS-TRTOT ).
  vlr_total = wg_header-vlrtot.

  TRY .
      vta_sistpro = ( vta_bruto - ( vta_bruto * ( inss / 100 ) ) - ( vta_bruto * ( fundeinfra / 100 ) ) ) - ( fethab * vlr_trotot ).
    CATCH cx_sy_zerodivide.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  RECALCULO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM recalculo.

  DATA: wl_tot_vlr   TYPE zsdt0041-zwert,
        total_ajuste TYPE zdmbtr,
        diferenca    TYPE p DECIMALS 2,
        resto        TYPE p DECIMALS 2.

  diferenca = vta_sistpro - vlr_total.

  SORT tg_itens BY zmeng.

  LOOP AT tg_itens ASSIGNING FIELD-SYMBOL(<itens>).

    TRY .
        <itens>-vlr_ajuste = ( <itens>-vlrtot / vlr_total ) * diferenca.
      CATCH cx_sy_zerodivide.
    ENDTRY.

    ADD <itens>-vlr_ajuste TO total_ajuste.

    IF lines( tg_itens[] ) EQ sy-tabix.
      IF total_ajuste NE diferenca.
        resto = diferenca - total_ajuste.
        ADD resto TO <itens>-vlr_ajuste.
      ENDIF.
    ENDIF.

    ADD <itens>-vlr_ajuste TO <itens>-vlrtot.

  ENDLOOP.

*  BREAK-POINT.
*    Realiza o calculo do campo "Valor Total"
  PERFORM vlrtot.
  PERFORM grava_dados.
*  PERFORM CALCULA_ITENS.
*  PERFORM CALCULA_HEADER USING WL_TOT_VLR.

  SORT tg_itens BY posnr.

  PERFORM log_acao USING 'W' '' CHANGING lv_erro.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REAJUSTE_41
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reajuste_41 .

  DATA vl_dif TYPE netwr_ap.
  DATA vl_tot TYPE netwr_ap.


  SELECT *
    FROM zsdt0041
    INTO TABLE @DATA(it_0041_aux)
    WHERE doc_simulacao EQ @wg_header-doc_simulacao.

  CHECK it_0041_aux IS NOT INITIAL.

  SELECT *
    FROM vbap
     INTO TABLE @DATA(it_vbap)
     FOR ALL ENTRIES IN @it_0041_aux
    WHERE vbeln EQ @it_0041_aux-vbeln
      AND matnr EQ @it_0041_aux-matnr.

  LOOP AT it_0041_aux INTO DATA(l_0041).
    LOOP AT it_vbap INTO DATA(l_vbap) WHERE vbeln EQ l_0041-vbeln AND matnr EQ l_0041-matnr.

      SELECT SINGLE knumv
        FROM vbak
        INTO @DATA(vl_knumv)
        WHERE vbeln EQ @l_vbap-vbeln.


*---> 19/07/2023 - Migração S4 - DG
*      SELECT SINGLE kbetr
*        FROM konv
*        INTO @DATA(vl_kbetr)
*        WHERE knumv EQ @vl_knumv
*          AND kposn EQ @l_vbap-posnr
*          AND kschl EQ 'RB00'.

      SELECT SINGLE kbetr
        FROM v_konv
        INTO @DATA(vl_kbetr)
        WHERE knumv EQ @vl_knumv
          AND kposn EQ @l_vbap-posnr
          AND kschl EQ 'RB00'.
*<--- 19/07/2023 - Migração S4 - DG


      vl_tot = 0.
      vl_dif = 0.
      vl_tot = l_vbap-netwr + l_vbap-mwsbp.

      IF vl_tot NE l_0041-vlrtot.

        vl_dif = l_0041-vlrtot - vl_tot.

        ADD vl_kbetr TO vl_dif.

        APPEND VALUE #(
                        vbeln         = l_vbap-vbeln
                        matnr         = l_vbap-matnr
                        desc_absoluto = vl_dif
                      ) TO it_zsdt0041_ajuste.

        CALL FUNCTION 'ZSDMF001_ATUALI_OV_SIMULADOR_2'
          TABLES
            ti_itens_ov       = it_zsdt0041_ajuste
          EXCEPTIONS
            ov_nao_encontrada = 1
            OTHERS            = 2.

        FREE it_zsdt0041_ajuste.

      ENDIF.
    ENDLOOP.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_FUNRURAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_funrural.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
*  SELECT SINGLE funrural
*    INTO wg_header-funrural
*    FROM zsdt0040
*    WHERE doc_simulacao EQ wg_header-doc_simulacao.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  IF wg_header-funrural IS INITIAL.
    wg_header-funrural = abap_true.
  ELSE.
    wg_header-funrural = abap_false.
  ENDIF.

  wg_header-funuser  = sy-uname.
  wg_header-fundata  = sy-datum.
  wg_header-funhora  = sy-uzeit.

  UPDATE zsdt0040 SET funrural = wg_header-funrural
                      funuser  = wg_header-funuser
                      fundata  = wg_header-fundata
                      funhora  = wg_header-funhora
   WHERE doc_simulacao EQ wg_header-doc_simulacao.

  PERFORM log_acao  USING 'F' '' CHANGING lv_erro.
ENDFORM.

* Início - CS2019000925 - Sara Oikawa - Julho/2020
*&---------------------------------------------------------------------*
*&      Form  DISPARA_HEDGE_AJUSTE_SACAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM dispara_hedge_ajuste_sacas TABLES p_tl_input_0041
                         USING  p_wl_input_0040 p_wl_tipo.

******************************************************************************
**** TABELA ZSDT0041 COM MATKL PARA AGRUPAR OS DADOS PELO GRUPO DE MERCADORIA
******************************************************************************
  TYPES BEGIN OF ty_0041.
  INCLUDE TYPE zsdt0041.
  TYPES matkl TYPE matkl.
  TYPES brgew TYPE brgew.
  TYPES dtpgtcult TYPE bapi_jbd_dte_dzterm.
  TYPES kursk TYPE kursk.
  TYPES END OF ty_0041.

***************************************************
**** OBJ DA TAXA CAMBIO E CURVA E OS METODOS DA 94
***************************************************
  DATA: obj_tx_curva TYPE REF TO zcl_webservice_tx_curva,
        obj_insere   TYPE REF TO zcl_taxa_curva_db,
        obj_0094     TYPE REF TO zcl_taxa_curva.

******************************************
********  TABELAS INTERNAS E WORK AREAS
******************************************
  DATA: it_0041        TYPE TABLE OF ty_0041,
        wa_0040        TYPE zsdt0040,
        tipo           TYPE char3,
        var_total      TYPE dmbtr,
        sequencia      TYPE posnr,
        taxa           TYPE kursf,
        taxa_0090      TYPE kursf,
        vl_qtde_equal  TYPE c,
        brgew          TYPE brgew,
        v_liq          TYPE netwr,
        v_imp          TYPE mwsbp,

        set_porc_frete TYPE p DECIMALS 5,
        vlr_frete      TYPE zsdt0037-vlr_frete,
        um_frete       TYPE zsdt0037-meins,
        vlr_frete_v    TYPE zsdt0037-vlr_frete,
        um_frete_v     TYPE zsdt0037-meins.

  DATA: it_set TYPE TABLE OF rgsb4,
        wa_set TYPE rgsb4.


  FIELD-SYMBOLS <final> TYPE ty_0041.

**********************
****** LIBERA OS OBJ
**********************
  FREE: obj_tx_curva, obj_0094, obj_insere.

**********************
****** CRIA OS OBJ
**********************
  CREATE OBJECT: obj_tx_curva, obj_0094, obj_insere.

  CLEAR taxa.

  wa_0040 = p_wl_input_0040.

  tipo = p_wl_tipo.

  CASE tipo.

    WHEN 'VDI'. "Venda mercado Interno

**********************************************************************************
* MONTA A ESTRUTURA COM OS DADOS AGRUPADOS SOMADOS E DIVIDIDOS PELO INCO1 E MATKL
**********************************************************************************
      obj_0094->agrupa_dados( EXPORTING  i_numero = wa_0040-doc_simulacao
                             i_tipo   = tipo
*                            T_ITENS  = T_ITENS
                              IMPORTING i_0041 = it_0041
                             ).

*        SELECT SINGLE * FROM ZSDT0040
*          INTO @DATA(WA_0040)
*          WHERE DOC_SIMULACAO EQ @I_NUMERO.

*    MONTA A SAIDA PARA INCLUIR NO DISPARO
      tipo = p_wl_tipo.
      LOOP AT it_0041 ASSIGNING <final>.

        IF <final>-vlr_ajuste IS INITIAL.
          CONTINUE.
        ENDIF.

        obj_0094->set_numero( <final>-doc_simulacao ).

        obj_0094->set_data_venc( <final>-dtpgtcult ).

        obj_0094->set_data_lib( sy-datum ).

        var_total = <final>-vlr_ajuste.

        obj_0094->set_total_proporcional( var_total ).
*        OBJ_0094->TIPO_TAXA_IN( TIPO ).
        obj_0094->tipo_taxa_in( '' ).


        " 19.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- >
        TRY .

            obj_0094->set_taxa_curva(
            obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                                       i_data_lib = obj_0094->get_data_lib(  )
                                       i_tipo     = obj_0094->get_tipo_taxa( )
                                     ) ).

          CATCH zcx_webservice .

            MESSAGE w104(zsd).

        ENDTRY.
        " 19.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- >

        obj_0094->set_tipo( tipo ).
        obj_0094->set_incoterms( <final>-inco1 ).
        obj_0094->set_matkl( i_matkl = <final>-matkl
                             i_brgew = <final>-brgew
                           ).
        obj_0094->set_bezei( '' ).
        obj_0094->set_cadencia_in( i_cadencia =  <final>-zmeng
                                   i_negativa = 'N'
                                 ).

        obj_0094->set_taxa_in( ).
        IF wa_0040-tpsim EQ 'VF'.
          obj_0094->set_taxa_cambio( wa_0040-kursf ).
        ELSE.
          IF obj_0094->get_taxa_in( ) IS INITIAL.
            obj_0094->set_taxa_cambio( obj_0094->get_taxa_curva( ) ).
          ELSE.
            obj_0094->set_taxa_cambio( obj_0094->get_taxa_in( ) ).
          ENDIF.
        ENDIF.

        obj_insere->zif_taxa_curva_db~inserir( obj_taxa = obj_0094 ).

      ENDLOOP.

    WHEN 'FRI'. "Frete Mercado interno

      CALL FUNCTION 'G_SET_GET_ALL_VALUES'
        EXPORTING
          class           = '0000'
          setnr           = 'MAGGI_FRI_HEDGE'
          no_descriptions = space
          no_rw_info      = space
        TABLES
          set_values      = it_set
        EXCEPTIONS
          set_not_found   = 1
          OTHERS          = 2.

      IF sy-subrc IS INITIAL.

        wa_set = it_set[ 1 ].

        CALL FUNCTION 'MOVE_CHAR_TO_NUM'
          EXPORTING
            chr             = wa_set-from
          IMPORTING
            num             = set_porc_frete
          EXCEPTIONS
            convt_no_number = 1
            convt_overflow  = 2
            OTHERS          = 3.

        set_porc_frete = set_porc_frete / 100.

      ENDIF.

*****************************
***** CABEÇALHO DA SIMULAÇÃO
*****************************
*        SELECT SINGLE * FROM ZSDT0040
*          INTO WA_0040
*          WHERE DOC_SIMULACAO EQ I_NUMERO.

      SELECT SINGLE * FROM zsdt0117
        INTO @DATA(wl_0117)
        WHERE bukrs      EQ @wa_0040-vkorg
          AND desativado EQ @abap_false.

      wa_0040-dt_entrega_sem = wa_0040-dt_entrega_sem + 30.
      wa_0040-dt_entrega_def = wa_0040-dt_entrega_def + 30.
      wa_0040-dt_entrega_fet = wa_0040-dt_entrega_fet + 30.

**********************************************************************************
* MONTA A ESTRUTURA COM OS DADOS AGRUPADOS SOMADOS E DIVIDIDOS PELO INCO1 E MATKL
**********************************************************************************
      obj_0094->agrupa_dados( EXPORTING  i_numero = wa_0040-doc_simulacao
                              i_tipo   = tipo
*                             T_ITENS  = T_ITENS
                              IMPORTING i_0041 = it_0041
                             ).

      LOOP AT it_0041 ASSIGNING <final>.

        IF <final>-vlr_ajuste IS INITIAL.
          CONTINUE.
        ENDIF.

        obj_0094->set_numero( <final>-doc_simulacao ).

        obj_0094->set_matkl( i_matkl = <final>-matkl
                             i_brgew = <final>-brgew
                           ).

        IF <final>-spart EQ '03'.

          IF ( wa_0040-waerk = 'USD' ) AND ( wl_0117-kursk IS NOT INITIAL ).
            var_total = ( <final>-vlr_ajuste * set_porc_frete ) * wl_0117-kursk.
          ELSE.
            var_total = <final>-vlr_ajuste * set_porc_frete.
          ENDIF.

        ELSE.
          CONTINUE.
        ENDIF.


        obj_0094->set_bezei( '' ).
        obj_0094->set_cadencia_in( i_cadencia =  <final>-zmeng
                                   i_negativa = 'S'
                                   i_0040 = wa_0040
                                 ).
        obj_0094->set_data_lib( sy-datum ).
        obj_0094->set_total_proporcional( i_total =  var_total
                                          i_negativa = abap_true
                                         ).
*        OBJ_0094->TIPO_TAXA_IN( TIPO ).
        obj_0094->tipo_taxa_in( '' ).



        " 19.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- >
        TRY .

            obj_0094->set_taxa_curva(
            obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                                       i_data_lib = obj_0094->get_data_lib(  )
                                       i_tipo     = obj_0094->get_tipo_taxa( )
                                     ) ).
          CATCH zcx_webservice .

            MESSAGE w104(zsd).

        ENDTRY.
        " 19.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- >

        obj_0094->set_taxa_cambio( <final>-kursk ).
        obj_0094->set_tipo( tipo ).
        obj_0094->set_incoterms( <final>-inco1 ).
        obj_insere->zif_taxa_curva_db~inserir( obj_taxa = obj_0094 ).

      ENDLOOP.


  ENDCASE.

ENDFORM.
* Fim - CS2019000925 - Sara Oikawa - Julho/2020

*Início - Sara Oikawa - 38859 - Agosto/2020
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_RECEBIMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_verifica_recebimentos .

  TYPES: BEGIN OF ty_zfit0026,
           vbeln TYPE vbeln,
         END OF ty_zfit0026.

  DATA: lt_zfit0026 TYPE TABLE OF ty_zfit0026,
        ls_zfit0026 TYPE ty_zfit0026,

        lv_msg      TYPE text255,
        lt_valuetab TYPE TABLE OF string,
        ls_valuetab TYPE string.

  CLEAR wg_flg_receb.

  IF tg_itens[] IS NOT INITIAL.
    SELECT vbeln
      FROM zfit0026
      INTO TABLE lt_zfit0026
      FOR ALL ENTRIES IN tg_itens
      WHERE vbeln EQ tg_itens-vbeln.
  ENDIF.

  IF tg_trans[] IS NOT INITIAL.
    SELECT vbeln
      FROM zfit0026
      APPENDING TABLE lt_zfit0026
      FOR ALL ENTRIES IN tg_trans
      WHERE vbeln EQ tg_trans-vbeln.
  ENDIF.

  DELETE lt_zfit0026 WHERE vbeln IS INITIAL.

  IF lt_zfit0026 IS NOT INITIAL.

    CLEAR: wg_save, sy-ucomm.
    wg_flg_receb = c_x.

    READ TABLE lt_zfit0026 INTO ls_zfit0026 INDEX 1.
    MESSAGE i001(zsd) DISPLAY LIKE 'E' WITH TEXT-e57 TEXT-e58 ls_zfit0026-vbeln.

*    LOOP AT LT_ZFIT0026 INTO LS_ZFIT0026.
*      LS_VALUETAB = LS_ZFIT0026-VBELN.
*      APPEND LS_VALUETAB TO LT_VALUETAB.
*    ENDLOOP.

  ENDIF.

*  IF LT_VALUETAB IS NOT INITIAL.
*
*    CLEAR WG_SAVE.
*    WG_FLG_RECEB = C_X.
*
*    CONCATENATE 'Erro. Alteração não possível!' 'Já existem recebimentos lançados.'
*                INTO LV_MSG SEPARATED BY SPACE.      "Title
*
*    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_TABLE'
*      EXPORTING
*        TITLEBAR        = LV_MSG
*        START_COLUMN    = 10
*        START_ROW       = 5
*        END_COLUMN      = 70
*        END_ROW         = 10
*        COLUMNNAME      = 'Ov´s em ZFIS26:'
**      IMPORTING
**       ANSWER          =
*      CHANGING
*        CT_DISPLAYTABLE = LT_VALUETAB.
*
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_LOG_EDICAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_log_edicao  TABLES p_tl_41 p_tl_41_old
                   USING  p_wl_40 p_wl_40_old.

  DATA: wl_40         TYPE          zsdt0040,
        wl_40_old     TYPE          zsdt0040,

        tl_41         TYPE TABLE OF zsdt0041,
        wl_41         TYPE          zsdt0041,

        tl_41_old     TYPE TABLE OF zsdt0041,
        wl_41_old     TYPE          zsdt0041,

        fieldname(30) TYPE c.

  wl_40       = p_wl_40.
  wl_40_old   = p_wl_40_old.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  " Caso Docto em inclusão, não gravar LOG
  CHECK  wl_40_old-doc_simulacao IS NOT INITIAL.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  tl_41[]     = p_tl_41[].
  tl_41_old[] = p_tl_41_old[].

  REFRESH tg_log_edicao.

* Grava Log Edição - Header
  DATA(t_field) = zcl_solicitacao_ov=>get_fieldname_structure( wl_40 ).
  LOOP AT t_field INTO DATA(_field).
    fieldname = |WL_40-{ _field-name }|.
    PERFORM input_log_ed USING wl_40     wl_40_old
                               wl_41     wl_41_old
                               fieldname 'HEADER'  '0'.
  ENDLOOP.


* Grava Log Edição - Itens
  t_field = zcl_solicitacao_ov=>get_fieldname_structure( wl_41 ).
  LOOP AT tl_41 INTO wl_41.
*    WL_TABIX_AUX = SY-TABIX.

    CLEAR: wl_41_old.
    READ TABLE tl_41_old INTO wl_41_old WITH KEY posnr = wl_41-posnr.

    LOOP AT t_field INTO _field.
      fieldname = |WL_41-{ _field-name }|.
      PERFORM input_log_ed USING wl_40     wl_40_old
                                 wl_41     wl_41_old
                                 fieldname 'ITENS'  wl_41-posnr.
    ENDLOOP.

  ENDLOOP.

  IF tg_log_edicao IS NOT INITIAL.
    MODIFY zsdt0091 FROM TABLE tg_log_edicao.
    COMMIT WORK.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INPUT_LOG_ED
*&---------------------------------------------------------------------*
FORM input_log_ed  USING    wl_40
                            wl_40_old
                            wl_41
                            wl_41_old
                            p_campo
                            p_area
                            p_item .

  DATA: wl_40_i           TYPE zsdt0040,
        wl_field(30),
        wl_field_old(40),
        wl_field_aux(40),
        wl_field_aux2(40).
  FIELD-SYMBOLS: <fs_field>     TYPE any,
                 <fs_field_old> TYPE any.

  wl_40_i = wl_40.

  UNASSIGN <fs_field>.
  UNASSIGN <fs_field_old>.

  wl_field = p_campo.
  SPLIT wl_field AT '-' INTO wl_field_aux
                             wl_field_aux2.

  IF wl_field_aux2 NE 'MANDT'      AND wl_field_aux2 NE 'USNAM'  AND
     wl_field_aux2 NE 'DATA_ATUAL' AND wl_field_aux2 NE 'HORA_ATUAL' AND
     wl_field_aux2 NE 'JOB'        AND wl_field_aux2 NE 'FUNDATA'    AND
     wl_field_aux2 NE 'FUNHORA'    AND wl_field_aux2 NE 'FUNUSER'.

    CONCATENATE wl_field_aux '_OLD-' wl_field_aux2 INTO wl_field_old.
    ASSIGN (wl_field) TO <fs_field>.
    ASSIGN (wl_field_old) TO <fs_field_old>.
    IF <fs_field> IS ASSIGNED.
      IF <fs_field> NE <fs_field_old>.

        SPLIT p_campo AT '-' INTO wl_field
                                  wl_field_aux.

        MOVE: wl_40_i-doc_simulacao    TO wg_log_edicao-doc_simulacao,
              p_item                   TO wg_log_edicao-item,
              tg_log_acao-id_historico TO wg_log_edicao-id_historico,
              p_area                   TO wg_log_edicao-area,
              wl_field_aux             TO wg_log_edicao-campo,
              <fs_field>               TO wg_log_edicao-new_value,
              <fs_field_old>           TO wg_log_edicao-old_value,
              sy-uname                 TO wg_log_edicao-usnam,
              sy-datum                 TO wg_log_edicao-data_atual,
              sy-uzeit                 TO wg_log_edicao-hora_atual.
        APPEND wg_log_edicao TO tg_log_edicao.
        CLEAR: wg_log_edicao.

      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " INPUT_LOG
*Fim - Sara Oikawa - 38859 - Agosto/2020

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_QTD_EMBALAGEM
*&---------------------------------------------------------------------*
*   Verificação da Quantidade da Embalagem (Somente para Defensivos)
*----------------------------------------------------------------------*
FORM zf_busca_qtd_embalagem_zdef .

  CLEAR:  wg_matnr, wg_groes, wg_meins , wg_groes_string, wg_groes_dec.

  SELECT SINGLE matnr groes meins
    FROM mara
    INTO ( wg_matnr, wg_groes, wg_meins )
   WHERE matnr EQ tg_itens-matnr.

  IF sy-subrc IS INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = tg_itens-matnr
      IMPORTING
        output = wg_matnr.

    IF wg_groes IS NOT INITIAL.

      wg_groes_string = wg_groes.

      IF wg_groes CA '0123456789,'.
        CALL FUNCTION 'C14W_CHAR_NUMBER_CONVERSION'
          EXPORTING
            i_string                   = wg_groes_string
          IMPORTING
*           E_FLOAT                    =
            e_dec                      = wg_groes_dec
*           E_DECIMALS                 =
          EXCEPTIONS
            wrong_characters           = 1
            first_character_wrong      = 2
            arithmetic_sign            = 3
            multiple_decimal_separator = 4
            thousandsep_in_decimal     = 5
            thousand_separator         = 6
            number_too_big             = 7
            OTHERS                     = 8.
        IF sy-subrc IS INITIAL.
          CLEAR wg_groes_string.
        ENDIF.
      ELSE.
        wg_groes_string = c_x.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

* Início - Sara Oikawa - 19.10.2020   Melhorias Pacote 5/6
*&---------------------------------------------------------------------*
*&      Module  PBO0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo0110 OUTPUT.

  SET PF-STATUS 'PF0110'.
  SET TITLEBAR  'TI0110'.

  wg_safra_0110_sg = wg_header-safra.
  CLEAR wg_idcompr_0110_sg.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI0110 INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai0110 INPUT.
  CASE sy-ucomm.
    WHEN 'SEARCH_SG'.
      IF wg_safra_0110_sg IS INITIAL AND wg_idcompr_0110_sg IS INITIAL.
        MESSAGE 'É obrigatório o preenchimento do campo "SAFRA" ou "ID Compra'  TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        IF wg_safra_0110_sg IS NOT INITIAL AND wg_idcompr_0110_sg IS NOT INITIAL.
          MESSAGE 'Informar apenas campo "SAFRA" ou campo "ID Compra'  TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.
          PERFORM zf_busca_dados_sigam.
        ENDIF.
      ENDIF.

    WHEN 'DELETE_SG'.
      IF  wg_0260-id_compra IS NOT INITIAL.
        CLEAR: wg_0260-id_compra,
               wg_0260-compra_fim_export.
      ENDIF.

      PERFORM zf_determine_funrural USING abap_true. "Forçar determinar Funrural

      LEAVE TO SCREEN 0.

    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_DADOS_SIGAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_busca_dados_sigam .

  DATA: lv_erro     TYPE char1,
        lv_cpf      TYPE stcd2,
        lv_cnpj     TYPE stcd1,
        ws_zsdt0260 TYPE zsdt0260.

  REFRESH: tg_sigam, tg_0260.
  CLEAR:   wg_sigam, ws_zsdt0260, lv_erro, lv_cpf, lv_cnpj.

  lv_cpf =  wg_cpf.
  lv_cnpj = wg_cnpj.

  IF wg_idcompr_0110_sg IS NOT INITIAL.
    CLEAR: wg_safra_0110_sg.
  ENDIF.

  IF wg_safra_0110_sg IS NOT INITIAL.
    CLEAR: wg_idcompr_0110_sg.
  ENDIF.

  CALL FUNCTION 'ZSDMF011_BUSCA_COMPRA_SIGAM'
    EXPORTING
      i_safra     = wg_safra_0110_sg
      i_id_compra = wg_idcompr_0110_sg
      i_cpf       = lv_cpf
      i_cnpj      = lv_cnpj
    IMPORTING
      e_zsdt0260  = tg_0260
      e_erro      = lv_erro.


*  IF lv_erro IS INITIAL.

**  SELECT * FROM zsdt0260
**    INTO CORRESPONDING FIELDS OF TABLE tg_sigam.

  LOOP AT tg_0260 INTO ws_zsdt0260.
    MOVE-CORRESPONDING ws_zsdt0260 TO wg_sigam.
    APPEND wg_sigam TO tg_sigam.
  ENDLOOP.

  IF tg_sigam IS NOT INITIAL.
    PERFORM zf_exibe_dados_sigam.
    LEAVE TO SCREEN 0.

  ELSE.

    " Nenhum ID Compra foi localizado para os dados informados.
    MESSAGE TEXT-e73 TYPE 'I' DISPLAY LIKE 'E'.

  ENDIF.

*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBE_DADOS_SIGAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_exibe_dados_sigam .

  DATA: wl_layout TYPE  slis_layout_alv.

  PERFORM zf_definir_eventos_0110.
  PERFORM zf_monta_layout_compra_sigam.

  v_report = sy-repid.

  wl_layout-zebra = c_x.
  wl_layout-box_fieldname   = 'MARK'.
  wl_layout-box_tabname     = 'TG_SIGAM'.
  wl_layout-window_titlebar = 'Compra SIGAM'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = v_report
      i_callback_user_command = 'ZF_XUSER_COMMAND_0110'
      is_layout               = wl_layout
      it_fieldcat             = estrutura[]
*     IT_SORT                 = T_SORT[]
      i_save                  = 'A'
      i_screen_start_column   = 4
      i_screen_start_line     = 9
      i_screen_end_column     = 100
      i_screen_end_line       = 15
    TABLES
      t_outtab                = tg_sigam.

ENDFORM.                    " ZF_EXIBE_DADOS_SIGAM


*&---------------------------------------------------------------------*
*&      Form  ZF_DEFINIR_EVENTOS_0110
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_definir_eventos_0110.

  PERFORM f_carregar_eventos USING:
                             slis_ev_user_command 'ZF_XUSER_COMMAND_0110'.

ENDFORM.                    " ZF_DEFINIR_EVENTOS_0110
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_LAYOUT_COMPRA_SIGAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_monta_layout_compra_sigam.
  REFRESH: estrutura.
  PERFORM montar_estrutura_matnr USING:
     1  'ZSDT0260'  'ID_COMPRA'          'TG_SIGAM'  'ID_COMPRA'          'ID Compra'        ' ',
     2  'ZSDT0260'  'COMPRA_FIM_EXPORT'  'TG_SIGAM'  'COMPRA_FIM_EXPORT'  'Fins Exportação'  ' ',
     3  'ZSDT0260'  'SAFRA'              'TG_SIGAM'  'SAFRA'              'Safra'            ' ',
     4  'ZSDT0260'  'STATUS'             'TG_SIGAM'  'STATUS'             'Status'           ' ',
     5  'ZSDT0260'  'ID_MATERIAL_SAP'    'TG_SIGAM'  'ID_MATERIAL_SAP'    'Produto'          ' ',
     6  'ZSDT0260'  'ID_FILIAL_SAP'      'TG_SIGAM'  'ID_FILIAL_SAP'      'Filial'           ' ',
     7  'ZSDT0260'  'DATA_COMPRA'        'TG_SIGAM'  'DATA_COMPRA'        'Data Compra'      ' ',
     8  'ZSDT0260'  'ID_FORNEC_SAP'      'TG_SIGAM'  'ID_FORNEC_SAP'      'Cod. Fornecedor'  ' ',
     9  'ZSDT0260'  'ID_CLIENTE_SAP'     'TG_SIGAM'  'ID_CLIENTE_SAP'     'Cod. Cliente'     ' '.

ENDFORM.                    " ZF_MONTA_LAYOUT_COMPRA_SIGAM

*---------------------------------------------------------------------*
*       FORM ZF_XUSER_COMMAND_0110                                    *
*---------------------------------------------------------------------*
* Trata Tela 0110 - Dados Compra SIGAM                                *
*---------------------------------------------------------------------*
FORM zf_xuser_command_0110 USING ucomm    LIKE sy-ucomm
                                 selfield TYPE kkblo_selfield.. "#EC CALLED

  DATA: tl_sigam_aux TYPE TABLE OF ty_sigam,
        wl_sigam     TYPE ty_sigam,
        wl_lines     TYPE sy-tabix.

  REFRESH: tl_sigam_aux.

  CASE sy-ucomm.

    WHEN '&ONT'.
      tl_sigam_aux[] = tg_sigam[].
      DELETE tl_sigam_aux WHERE mark IS INITIAL.
      DESCRIBE TABLE tl_sigam_aux LINES wl_lines.
      IF wl_lines EQ 0.
        " Selecionar uma linha.
        MESSAGE TEXT-e85  TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE SCREEN.
      ENDIF.
      IF wl_lines GT 1.
        " Selecionar apenas uma linha.
        MESSAGE TEXT-e86 TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE SCREEN.
      ELSE.
        READ TABLE tl_sigam_aux INTO  wl_sigam INDEX 1.
        MOVE-CORRESPONDING wl_sigam TO wg_0260.    "Tabela Dados Compra SIGAM
        PERFORM zf_valida_compra_sigam USING ' '
                                       CHANGING  tg_msg_ret-msg.

        PERFORM zf_determine_funrural USING abap_true. "Forçar determinar Funrural

      ENDIF.

      CALL METHOD grid1->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    WHEN 'CANCEL' OR 'EXIT'.
      LEAVE SCREEN.

  ENDCASE.

ENDFORM. "ZF_XUSER_COMMAND_0110

*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_COMPRA_SIGAM
*&---------------------------------------------------------------------*
*  Se p_log = ' ', mensagem na própria tela (Compra Sigam)
*  Se p_log = 'X', mensagem retorno no LOG de ERROS (FORM Verifica_erros)
*                  Colocar validações no LOG de ERROS, para garantir que
*                  após o usuário escolher o ID_COMPRA_SIGAM,
*                  caso ele altere as informações da
*                  "Cultura Pagamento" / "Safra Pagamento" "Escritório de Vendas",
*                  o sistema valide e não deixe Salvar, se houver inconsistência.
*                  Observado que o problema ocorrerá quando a venda
*                  for TS (Troca SAFRA)
*----------------------------------------------------------------------*
FORM zf_valida_compra_sigam USING    p_log
                            CHANGING p_msg TYPE char255.

  DATA:  lv_matkl  TYPE mara-matkl.

  wg_0260-doc_simulacao = wg_header-doc_simulacao.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input  = wg_0260-id_material_sap
    IMPORTING
      output = wg_0260-id_material_sap.

  SELECT SINGLE matkl
    INTO lv_matkl
     FROM mara
    WHERE matnr = wg_0260-id_material_sap.

  CLEAR p_msg.

  IF wg_0260-status NE 'EF'.
    " Compra deve estar com Status "Efetivado".
    p_msg = TEXT-e74.
    IF p_log IS INITIAL.
      CLEAR wg_0260.
      MESSAGE p_msg TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE SCREEN.
    ENDIF.
  ENDIF.

  IF wg_header-tpsim EQ 'TS'.
    IF wg_header-cultura = 'SJ' AND lv_matkl NE '700110'.
      " Material da Compra deve ser compatível com a Cultura do Simulador "Soja"
      CONCATENATE TEXT-e75 TEXT-e76 INTO p_msg.
      IF p_log IS INITIAL.
        CLEAR wg_0260.
        MESSAGE p_msg TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE SCREEN.
      ENDIF.
    ENDIF.

    IF wg_header-cultura = 'ML' AND lv_matkl NE '700170'.
      " Material da Compra deve ser compatível com a Cultura do Simulador "Milho"
      CONCATENATE TEXT-e75 TEXT-e77 INTO p_msg.
      IF p_log IS INITIAL.
        CLEAR wg_0260.
        MESSAGE p_msg TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE SCREEN.
      ENDIF.
    ENDIF.

    IF wg_header-cultura = 'AL' AND lv_matkl NE '700140'.
      " Material da Compra deve ser compatível com a Cultura do Simulador "Algodão"
      CONCATENATE TEXT-e75 TEXT-e78 INTO p_msg.
      IF p_log IS INITIAL.
        CLEAR wg_0260.
        MESSAGE p_msg TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE SCREEN.
      ENDIF.
    ENDIF.

    IF wg_0260-safra NE wg_header-safra.
      " Safra da Compra deve ser igual a Safra do Simulador.
      p_msg = TEXT-e79.
      IF p_log IS INITIAL.
        CLEAR wg_0260.
        MESSAGE p_msg TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE SCREEN.
      ENDIF.
    ENDIF.

    IF wg_0260-id_filial_sap NE wg_header-vkbur.
      " Filial da Compra deve ser igual ao Escritório de Venda do Simulador.
      p_msg = TEXT-e80.
      IF p_log IS INITIAL.
        CLEAR wg_0260.
        MESSAGE p_msg TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE SCREEN.
      ENDIF.
    ENDIF.

  ENDIF.

  IF wg_0260-id_cliente_sap IS INITIAL.
    CONCATENATE 'Compra Sigam Id:' wg_0260-id_compra 'sem Cliente SAP!' INTO p_msg SEPARATED BY space.
    IF p_log IS INITIAL.
      CLEAR wg_0260.
      MESSAGE p_msg TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE SCREEN.
    ENDIF.
  ENDIF.

  IF wg_header-kunnr IS INITIAL.
    p_msg = TEXT-e01.

    IF p_log IS INITIAL.
      CLEAR wg_0260.
      MESSAGE p_msg TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE SCREEN.
    ENDIF.
  ENDIF.

  SELECT SINGLE *
    FROM kna1 INTO @DATA(lwa_kna1_compra)
   WHERE kunnr EQ @wg_0260-id_cliente_sap.

  IF sy-subrc NE 0.
    CONCATENATE 'Cliente com Id:' wg_0260-id_cliente_sap 'não encontrado!' INTO p_msg SEPARATED BY space.
    IF p_log IS INITIAL.
      CLEAR wg_0260.
      MESSAGE p_msg TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE SCREEN.
    ENDIF.
  ENDIF.

  SELECT SINGLE *
    FROM kna1 INTO @DATA(lwa_kna1_sim)
   WHERE kunnr EQ @wg_header-kunnr.

  IF sy-subrc NE 0.
    CONCATENATE 'Cliente com Id:' wg_header-kunnr 'não encontrado!' INTO p_msg SEPARATED BY space.
    IF p_log IS INITIAL.
      CLEAR wg_0260.
      MESSAGE p_msg TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE SCREEN.
    ENDIF.
  ENDIF.

  IF NOT ( ( lwa_kna1_compra-stcd1 EQ lwa_kna1_sim-stcd1 ) AND
           ( lwa_kna1_compra-stcd2 EQ lwa_kna1_sim-stcd2 ) ).

    CONCATENATE 'Cliente Compra Id:' lwa_kna1_compra-kunnr 'não corresponde ao Emissor Ordem: Id:' lwa_kna1_sim-kunnr
           INTO p_msg SEPARATED BY space.

    IF p_log IS INITIAL.
      CLEAR wg_0260.
      MESSAGE p_msg TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE SCREEN.
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form ZF_DETERMINE_FUNRURAL
*&---------------------------------------------------------------------*
*   Determinar Funrural
*----------------------------------------------------------------------*
FORM zf_determine_funrural USING p_force TYPE c.

  REFRESH:  tg_lfa1,
            tg_funex.

  DATA(lva_funrural) = abap_false.

  IF ( p_force EQ abap_false ).

    IF wg_save NE c_altcpg.
      CHECK ( wg_acao  EQ c_add   OR sy-ucomm EQ c_add ) OR
            ( wg_acao  EQ c_modif OR sy-ucomm EQ c_add ).

      CHECK ( wg_header-kunnr <> wg_header_old-kunnr ).
    ENDIF.
  ENDIF.

  IF ( wg_0260-id_compra IS NOT INITIAL ) AND ( wg_0260-compra_fim_export = 'S' ).
    lva_funrural = abap_true.
  ELSE.

    IF wg_cnpj IS NOT INITIAL.
      SELECT lifnr sperr sperm sperq
        INTO TABLE tg_lfa1
        FROM lfa1
        WHERE stcd1 EQ wg_cnpj.
    ENDIF.

    IF wg_cpf IS NOT INITIAL.
      SELECT lifnr sperr sperm sperq
        INTO TABLE tg_lfa1
        FROM lfa1
        WHERE stcd2 EQ wg_cpf.
    ENDIF.

    "Elimina bloqueados
    DELETE tg_lfa1 WHERE sperr IS NOT INITIAL.
    DELETE tg_lfa1 WHERE sperm IS NOT INITIAL.
    DELETE tg_lfa1 WHERE sperq IS NOT INITIAL.

    IF tg_lfa1[] IS NOT INITIAL.
      SELECT * FROM zsdt0001funex
        INTO TABLE tg_funex
        FOR ALL ENTRIES IN tg_lfa1
      WHERE lifnr     EQ tg_lfa1-lifnr
        AND dt_inicio LE wg_header-erdat
        AND dt_final  GE wg_header-erdat.

      IF sy-subrc IS INITIAL.
        lva_funrural = abap_true.
      ENDIF.

    ENDIF.

  ENDIF.

  wg_header-funrural = lva_funrural.

  CLEAR: wg_header-funuser,
         wg_header-fundata,
         wg_header-funhora.

  wg_header_old-kunnr = wg_header-kunnr.

ENDFORM.

* Fim - Sara Oikawa - 19.10.2020   Melhorias Pacote 5/6

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
*&---------------------------------------------------------------------*
*&      Form  ZF_ALTERA_PAGTO_OV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_altera_pagto_ov .

  DATA:
    tl_itens_aux      TYPE TABLE OF ty_itens,
    wl_itens_aux      TYPE ty_itens,
    tl_0090           TYPE TABLE OF zsdt0090,
    wl_0090           TYPE zsdt0090,
    wl_orderheaderin  TYPE bapisdh1,
    wl_orderheaderinx TYPE bapisdh1x,
    tl_return         TYPE TABLE OF bapiret2 WITH HEADER LINE,

    lv_seq            TYPE zsdt0090-sequencia,
    lv_zterm_ant      TYPE vbkd-zterm,
    lv_zterm          TYPE vbkd-zterm.

  DATA: wl_0040 TYPE zsdt0040.

*  CLEAR _OK.

  tl_itens_aux[] = tg_itens[].
  DELETE tl_itens_aux WHERE vbeln IS INITIAL.
  SORT tl_itens_aux BY vbeln.
  DELETE ADJACENT DUPLICATES FROM tl_itens_aux COMPARING vbeln.

  CLEAR lv_seq.
  SELECT COUNT(*)
    FROM zsdt0090
    INTO lv_seq
   WHERE doc_simulacao EQ wg_header-doc_simulacao.

  CLEAR lv_zterm.
  IF wg_header-tpsim EQ 'TS'.
    lv_zterm = 'I001'.
  ELSEIF wg_header-tpsim EQ 'AD'.
    lv_zterm = 'I002'.
  ELSEIF wg_header-tpsim EQ 'TV'.
    lv_zterm = 'I004'.
  ENDIF.

  LOOP AT tl_itens_aux INTO wl_itens_aux.

    SELECT SINGLE zterm INTO lv_zterm_ant
      FROM vbkd
     WHERE vbeln EQ wl_itens_aux-vbeln.

    wl_orderheaderinx-updateflag = 'U'.

    wl_orderheaderin-pmnttrms    = lv_zterm.
    wl_orderheaderinx-pmnttrms   = abap_true.

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        salesdocument    = wl_itens_aux-vbeln
        order_header_in  = wl_orderheaderin
        order_header_inx = wl_orderheaderinx
      TABLES
        return           = tl_return.

    READ TABLE tl_return WITH KEY type = 'E'.
    IF NOT sy-subrc IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ENDIF.

    " Inserir um único Registro (não será por item de OV) na tabela ZSDT0090,
    " com ZSDT0090-CATEGORIA = (T - Alter. Cond. Pagamento), para identificar
    " todas as ordens do simulador que sofreram alteração da condição de pagamento.

    CLEAR wl_0090.

    ADD 1 TO lv_seq.

    wl_0090-doc_simulacao  = wg_header-doc_simulacao.
    wl_0090-sequencia      = lv_seq.
    wl_0090-vbelv          = wl_itens_aux-vbeln.
*   WL_0090-WERKSV
*   WL_0090-KUNNRV
    wl_0090-zterm          = lv_zterm.
    wl_0090-ztermv         = lv_zterm_ant.
    wl_0090-categoria      = 'T'.
    wl_0090-usnam          = sy-uname.
    wl_0090-data_atual     = sy-datum.
    wl_0090-hora_atual     = sy-uzeit.

    INSERT INTO zsdt0090  VALUES wl_0090.
    COMMIT WORK.

  ENDLOOP.

ENDFORM.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_DATE_RETROATIVA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_check_date_retroativa .

  IF wg_status NE icon_release.
    CASE wg_header-tpsim.
      WHEN 'AD'.
        IF wg_header-dt_entrega_fet IS NOT INITIAL AND wg_header-dt_entrega_fet < sy-datum OR wg_header-dt_entrega_sem IS NOT INITIAL AND wg_header-dt_entrega_sem < sy-datum OR wg_header-dt_entrega_def IS NOT INITIAL AND wg_header-dt_entrega_def <
sy-datum.
          APPEND VALUE #( msg = 'Data de entrega não pode ser menor que a data atual' ) TO tg_msg_ret.
        ENDIF.


        IF wg_header-dtpgtcult < sy-datum.
          APPEND VALUE #( msg = 'Data de pagamento não pode ser menor que a data atual' ) TO tg_msg_ret.
        ENDIF.

        IF wg_header-dtvencov < sy-datum.
          APPEND VALUE #( msg = 'Data de vencimento não pode ser menor que a data atual' ) TO tg_msg_ret.
        ENDIF.

      WHEN 'BN'.
        IF wg_header-dt_entrega_fet IS NOT INITIAL AND wg_header-dt_entrega_fet < sy-datum OR wg_header-dt_entrega_sem IS NOT INITIAL AND wg_header-dt_entrega_sem < sy-datum OR wg_header-dt_entrega_def IS NOT INITIAL AND wg_header-dt_entrega_def <
sy-datum.
          APPEND VALUE #( msg = 'Data de entrega não pode ser menor que a data atual' ) TO tg_msg_ret.
        ENDIF.


      WHEN 'PM'.
        IF wg_header-dt_entrega_fet IS NOT INITIAL AND wg_header-dt_entrega_fet < sy-datum OR wg_header-dt_entrega_sem IS NOT INITIAL AND wg_header-dt_entrega_sem < sy-datum OR wg_header-dt_entrega_def IS NOT INITIAL AND wg_header-dt_entrega_def <
sy-datum.
          APPEND VALUE #( msg = 'Data de entrega não pode ser menor que a data atual' ) TO tg_msg_ret.
        ENDIF.

        IF wg_header-dtpgtcult < sy-datum.
          APPEND VALUE #( msg = 'Data de pagamento não pode ser menor que a data atual' ) TO tg_msg_ret.
        ENDIF.

      WHEN 'TS'.
        IF wg_header-dt_entrega_fet IS NOT INITIAL AND wg_header-dt_entrega_fet < sy-datum OR wg_header-dt_entrega_sem IS NOT INITIAL AND wg_header-dt_entrega_sem < sy-datum OR wg_header-dt_entrega_def IS NOT INITIAL AND wg_header-dt_entrega_def <
sy-datum.
          APPEND VALUE #( msg = 'Data de entrega não pode ser menor que a data atual' ) TO tg_msg_ret.
        ENDIF.

        IF wg_header-dtpgtcult < sy-datum.
          APPEND VALUE #( msg = 'Data de pagamento não pode ser menor que a data atual' ) TO tg_msg_ret.
        ENDIF.

        IF wg_header-dtent < sy-datum.
          APPEND VALUE #( msg = 'Data de vencimento não pode ser menor que a data atual' ) TO tg_msg_ret.
        ENDIF.

      WHEN 'TV'.
        IF wg_header-dt_entrega_fet IS NOT INITIAL AND wg_header-dt_entrega_fet < sy-datum OR wg_header-dt_entrega_sem IS NOT INITIAL AND wg_header-dt_entrega_sem < sy-datum OR wg_header-dt_entrega_def IS NOT INITIAL AND wg_header-dt_entrega_def <
sy-datum.
          APPEND VALUE #( msg = 'Data de entrega não pode ser menor que a data atual' ) TO tg_msg_ret.
        ENDIF.

        IF wg_header-dtpgtcult < sy-datum.
          APPEND VALUE #( msg = 'Data de pagamento não pode ser menor que a data atual' ) TO tg_msg_ret.
        ENDIF.

        IF wg_header-dtent < sy-datum.
          APPEND VALUE #( msg = 'Data de Data de entrega não pode ser menor que a data atual' ) TO tg_msg_ret.
        ENDIF.


      WHEN 'VF'.
        IF wg_header-dt_entrega_fet IS NOT INITIAL AND wg_header-dt_entrega_fet < sy-datum OR wg_header-dt_entrega_sem IS NOT INITIAL AND wg_header-dt_entrega_sem < sy-datum OR wg_header-dt_entrega_def IS NOT INITIAL AND wg_header-dt_entrega_def <
sy-datum.
          APPEND VALUE #( msg = 'Data de entrega não pode ser menor que a data atual' ) TO tg_msg_ret.
        ENDIF.

      WHEN 'VP'.
        IF wg_header-dt_entrega_fet IS NOT INITIAL AND wg_header-dt_entrega_fet < sy-datum OR wg_header-dt_entrega_sem IS NOT INITIAL AND wg_header-dt_entrega_sem < sy-datum OR wg_header-dt_entrega_def IS NOT INITIAL AND wg_header-dt_entrega_def <
sy-datum.
          APPEND VALUE #( msg = 'Data de entrega não pode ser menor que a data atual' ) TO tg_msg_ret.
        ENDIF.

        IF wg_header-dtpgtcult < sy-datum.
          APPEND VALUE #( msg = 'Data de pagamento não pode ser menor que a data atual' ) TO tg_msg_ret.
        ENDIF.

        IF wg_header-dtinijuros < sy-datum.
          APPEND VALUE #( msg = 'Data de pagamento não pode ser menor que a data atual' ) TO tg_msg_ret.
        ENDIF.

      WHEN 'VV'.
        IF wg_header-dt_entrega_fet IS NOT INITIAL AND wg_header-dt_entrega_fet < sy-datum OR wg_header-dt_entrega_sem IS NOT INITIAL AND wg_header-dt_entrega_sem < sy-datum OR wg_header-dt_entrega_def IS NOT INITIAL AND wg_header-dt_entrega_def <
sy-datum.
          APPEND VALUE #( msg = 'Data de entrega não pode ser menor que a data atual' ) TO tg_msg_ret.
        ENDIF.

        IF wg_header-dtvencov < sy-datum.
          APPEND VALUE #( msg = 'Data de vencimento não pode ser menor que a data atual' ) TO tg_msg_ret.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_BOLETA
*&---------------------------------------------------------------------*
FORM f_popup_boleta CHANGING p_motivo TYPE zsded033
                             p_canc TYPE c.

  DATA ls_request TYPE zsde0026.
  DATA lt_boletas TYPE zsdc013.

  DATA ls_boleta TYPE zsde013.
  DATA ls_zsdt0308 TYPE zsdt0308.

  "DATA lv_erro TYPE c.
  DATA lv_motivo TYPE string.

  IF wg_header-tpsim = 'VV'.
    ls_request-bldat = wg_header-dtvencov.
  ELSE.
    ls_request-bldat = wg_header-dtpgtcult.
  ENDIF.

  DATA(lv_dat_ant) = ls_request-bldat.
  DATA(lv_dat_pos) = ls_request-bldat.

  SUBTRACT 5 FROM lv_dat_ant.
  ADD 5 TO lv_dat_pos.

  ls_request-bukrs = wg_header-vkorg.

  CLEAR gv_taxa_neg.

  CALL FUNCTION 'ZSDMF001_CONSULTA_BOLETA_API'
    EXPORTING
      iv_doc_simu = wg_header-doc_simulacao
      is_request  = ls_request
    IMPORTING
      et_boletas  = lt_boletas.

  " Mostrar somente boletas que possuem saldo disponível maior ou igual ao 'valor total-VLRTOT' do simulador.
  DELETE lt_boletas WHERE saldo < wg_header-vlrtot. "#debug ramon

*  " Mostrar somente boletas com um range de datas 5 dias anterior e 5 dia posterior a 'data de vencimento-DTPGTCULT' do simulador.
  "DELETE lt_boletas WHERE data_venc < lv_dat_ant.
  "DELETE lt_boletas WHERE data_venc > lv_dat_pos.

  CALL FUNCTION 'ZSDMF001_POPUP_BOLETA'
    EXPORTING
      iv_edit   = 'X'
      iv_venc   = wg_header-dtpgtcult
    IMPORTING
      es_line   = ls_boleta
      ev_motivo = lv_motivo
      ev_cancel = p_canc
    TABLES
      ct_boleta = lt_boletas.

  "break rblima.

  CHECK p_canc IS INITIAL AND ls_boleta IS NOT INITIAL.

  p_motivo = lv_motivo.

  MOVE-CORRESPONDING ls_boleta TO ls_zsdt0308.

  ls_zsdt0308-vlr_brl_enviado = wg_header-vlrtot.


  CALL FUNCTION 'ZSDMF001_REVERTE_BOLETA_API'
    IMPORTING
      ev_erro           = p_canc
    CHANGING
      cs_request        = ls_zsdt0308
    EXCEPTIONS
      falha_comunicacao = 1
      OTHERS            = 2.

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

  CHECK p_canc IS INITIAL.

  MODIFY zsdt0308 FROM ls_zsdt0308.

  COMMIT WORK AND WAIT.

  gv_taxa_neg = ls_zsdt0308-taxa_neg.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CALL_BOLETA_REPORT
*&---------------------------------------------------------------------*
FORM f_call_boleta_report.
*jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj

  DATA lr_simu TYPE RANGE OF zsded003.
ENHANCEMENT-POINT ZTESTE001 SPOTS ZTESTE .

ENHANCEMENT-POINT ZTESTE003 SPOTS ZTESTE .

ENHANCEMENT-POINT ZTESTE004 SPOTS ZTESTE .

  CHECK wg_header-doc_simulacao IS NOT INITIAL.

  APPEND 'IEQ' && wg_header-doc_simulacao TO lr_simu.

  SUBMIT zsdr0146 WITH so_simu IN lr_simu AND RETURN.
ENHANCEMENT-POINT ZTESTE002 SPOTS ZTESTE .

include zsdr016_0001 if FOUND.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema .

  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INIT_ADD
*&---------------------------------------------------------------------*
FORM f_init_add .

  DATA lv_answer.
  DATA lv_ecomm.

  PERFORM f_check_user_ecomm CHANGING lv_ecomm.

  CHECK lv_ecomm = 'X'.

  CALL FUNCTION 'ZSDMF_POPUP_RADIO_OPTIONS'
    EXPORTING
      iv_question = 'Qual a Origem da Venda?'
      iv_text01   = 'Convencional'
      iv_text02   = 'E-commerce'
    IMPORTING
      ev_answer   = lv_answer.

  IF lv_answer = '0' OR lv_answer = '1'.
    EXIT.
  ENDIF.

  wg_header-ecommerce = 'X'.

  wg_header-vendedor = 'E00'. " (E-commerce)
  wg_header-tpsim = 'VV'. "(Venda a Vista)
  wg_header-waerk = 'BRL'.
  wg_header-hbkid = 'AL5'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_USER_ECOMM
*&---------------------------------------------------------------------*
FORM f_check_user_ecomm CHANGING p_ecomm TYPE c.

  DATA lt_users_ecomm TYPE TABLE OF rgsb4.

  CLEAR p_ecomm.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_ZSDT0044_08'
    TABLES
      set_values    = lt_users_ecomm
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  READ TABLE lt_users_ecomm ASSIGNING FIELD-SYMBOL(<fs_line>)
    WITH KEY from = sy-uname.

  CHECK sy-subrc EQ 0.

  p_ecomm = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  SCREEN_108  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_108 OUTPUT.

  LOOP AT SCREEN.

    IF wg_header-ecommerce = 'X'.

      screen-input = 1.
    ELSE.
      screen-input = 0.
    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form atualiza_fundeinfra
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM atualiza_fundeinfra .
  IF wg_header-fundeinfra_exce IS INITIAL.
    wg_header-fundeinfra_exce = abap_true.
  ELSE.
    wg_header-fundeinfra_exce = abap_false.
  ENDIF.

  UPDATE zsdt0040 SET fundeinfra_exce = wg_header-fundeinfra_exce
   WHERE doc_simulacao EQ wg_header-doc_simulacao.

  PERFORM log_acao  USING 'F' '' CHANGING lv_erro.
ENDFORM.
