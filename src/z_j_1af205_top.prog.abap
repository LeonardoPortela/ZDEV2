*----------------------------------------------------------------------*
* Include Z_J_1AF205_TOP
*----------------------------------------------------------------------*

* Tipos de datos ------------------------------------------------------*
 TYPES:
    BEGIN OF ty_sal,
    lineno(6)        TYPE c,
    budat(8)         TYPE c,
    brnch            LIKE bkpf-brnch,
    belnr(10)        TYPE c,
    koart            LIKE bseg-koart,
    hkont(10)        TYPE c,
    name1            LIKE lfa1-name1,
    stcdt            LIKE lfa1-stcdt,
    stcd1            LIKE lfa1-stcd1,
    bldat(8)         TYPE c,
    xblnr(14)        TYPE c,
    oftp_text        LIKE zmmt_ee_zgr-text2, "j_1aoftpt-text5,
    augdt(8)         TYPE c,
    augbl            LIKE bseg-augbl,
    total(57)        TYPE c,
    cai(14)          TYPE c, "For magn. output: Print Authorization Code
    fisc_cont        TYPE c, "For magn. output:Fiscal Contrlr " RG1361
    mwskz(6)         TYPE c,
    rate(8)          TYPE c,
    taxed            LIKE bseg-wrbtr, "ALRS
    taxed2           LIKE bseg-wrbtr,
    taxed3           LIKE bseg-wrbtr,
    not_taxed        LIKE bseg-wrbtr,
    vat              LIKE bseg-wrbtr,
    rnr_vat          LIKE bseg-wrbtr,
    vat_percep       LIKE bseg-wrbtr,
    other_percep     LIKE bseg-wrbtr,
    exemption        LIKE bseg-wrbtr,
    exports          LIKE bseg-wrbtr,
    percepnoc        LIKE bseg-wrbtr,
    iother_percep01  TYPE bseg-wrbtr,
    iother_percep02  TYPE bseg-wrbtr,
    iother_percep03  TYPE bseg-wrbtr,
*** US #181035 - MMSILVA - 10.06.2025 - Ini ***
    iother_percep04  TYPE bseg-wrbtr,
    iother_percep05  TYPE bseg-wrbtr,
*** US #181035 - MMSILVA - 10.06.2025 - Fim ***

    line_total       LIKE bseg-wrbtr,
    exempt(16)       TYPE c, " Exempt reason for magnetic output
    earn_per(18)     TYPE c, " Note 645449
    buper            TYPE buper,
    END OF ty_sal.

* Tablas internas / Estructuras ---------------------------------------*
 DATA: t_sal   TYPE STANDARD TABLE OF ty_sal,
       t_sal_a TYPE STANDARD TABLE OF ty_sal,
       e_sal   LIKE LINE OF t_sal,
       e_sal_a LIKE LINE OF t_sal.

* Definiciones de Control ALV -----------------------------------------*
 TYPE-POOLS: kkblo.
* Variáveis a serem usadas na execução da função
 DATA: v_pos      TYPE i,
       v_status   TYPE slis_formname,
       v_user     TYPE slis_formname,
       v_top      TYPE slis_formname,
       t_variant  TYPE disvariant.
* FIELD CATALOG - Contém o formato de saída dos dados.
 DATA: t_afield   TYPE kkblo_fieldcat,
       t_fieldcat TYPE kkblo_t_fieldcat,
       t_fcat     TYPE slis_t_fieldcat_alv.
* LAYOUT - Determina as propriedades de controle da lista.
 DATA: t_layout   TYPE kkblo_layout,
       t_lay      TYPE slis_layout_alv.
* HEADER - Contém os campos do cabeçalho do relatório.
 DATA: t_head     TYPE kkblo_listheader.
 DATA: t_header   TYPE kkblo_t_listheader.
* EVENTS - Contém o nome doe eventos à serem executados.
 DATA: t_event    TYPE slis_t_event,
       t_events   TYPE slis_alv_event.
* SORT - Contém os criterios de ordenação para a lista de dados.
 DATA: t_sort    TYPE slis_t_sortinfo_alv WITH HEADER LINE.
* OUT - Contém os dados selecionados para a saída do relatório.
 DATA: BEGIN OF t_out OCCURS 0.
 DATA:  box     TYPE c.
         INCLUDE STRUCTURE e_sal.
 DATA:  colinfo TYPE kkblo_t_specialcol,
       END OF t_out.
* Constantes com a lista de eventos a serem executados no programa
 CONSTANTS : c_top    TYPE slis_formname VALUE 'TOP_OF_PAGE',
             c_user   TYPE slis_formname VALUE 'USER_COMMAND',
             c_list   TYPE slis_formname VALUE 'TOP_OF_LIST'.

 data: l_print type slis_print_alv.
