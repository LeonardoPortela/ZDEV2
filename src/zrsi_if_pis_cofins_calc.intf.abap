interface ZRSI_IF_PIS_COFINS_CALC
  public .


  interfaces IF_BADI_INTERFACE .

  types:
    mty_base(12) TYPE p DECIMALS 6 .
  types:
    mty_taxrate(12) TYPE p DECIMALS 6 .
  types:
    mty_taxamount(12) TYPE p DECIMALS 6 .
  types:
    BEGIN OF mty_tax_data,
      net_amount            TYPE kawrt,
      wt_base_amount        TYPE kawrt,
      discount              TYPE kwert,
      exempt                TYPE kwert,                     "2542287
      exempt_excipi         TYPE kwert,                     "2542287
      subtribmod            TYPE kwert,
      freight               TYPE kwert,
      caller(2)             TYPE c,
      cofinsrate            TYPE mty_taxrate,
      cofinsbase            TYPE mty_taxrate,
      cofinspauta           TYPE komv,
      calcasset             TYPE xfeld,                     "1755461
      icmsnormrate          TYPE mty_taxrate,
      icmsnormrate_intra    TYPE mty_taxrate,
      icmsfreightrate       TYPE mty_taxrate,
      icmsfreightrate_intra TYPE mty_taxrate,
      icmssubtribexceptrate TYPE mty_taxrate,
      icmssubtribrate       TYPE mty_taxrate,
      icmsrate              TYPE mty_taxrate,
      icmsrate_intra        TYPE mty_taxrate,
      icmsbase_intra        TYPE mty_taxrate,               "2232757
      icmsfcpbase           TYPE mty_taxrate,          "2443042 2232757
      icmsfcprate           TYPE mty_taxrate,          "2443042 2232757
      icmspartilhaexempt    TYPE xfeld,                     "2283474
      icmsfcpresale         TYPE xfeld,                     "2443042
      icmsbase              TYPE mty_taxrate,
      icmsconv100           TYPE xfeld,
      icmsother             TYPE xfeld,
      icmslaw               TYPE j_1btaxlw1,
      icmsnullexempt        TYPE xfeld,
      icmscompbase          TYPE mty_taxrate,
      icmscompfcpbase       TYPE mty_taxrate,               "2443042
      icmscompexempt        TYPE xfeld,
      icmscustexempt        TYPE xfeld,         "Note 958243
      ipirate               TYPE mty_taxrate,
      ipibase               TYPE mty_taxrate,
      ipipauta              TYPE komv,
      ipiother              TYPE xfeld,
      ipilaw                TYPE j_1btaxlw2,
      ipinullexempt         TYPE xfeld,
      ipicustexempt         TYPE xfeld,         "Note 958243
      issrate               TYPE mty_taxrate,
      issrate_prov          TYPE mty_taxrate,
      issrate_serv          TYPE mty_taxrate,
      issbase_prov          TYPE mty_taxrate,
      issbase_serv          TYPE mty_taxrate,
      isslaw_prov           TYPE j_1btaxlw3,
      isslaw_serv           TYPE j_1btaxlw3,
      isstaxrelloc_prov     TYPE j_1btaxrelloc,
      isstaxrelloc_serv     TYPE j_1btaxrelloc,
      isswithhold_prov      TYPE xfeld,
      isswithhold_serv      TYPE xfeld,
      issminval_wt_prov     TYPE komv,
      issminval_wt_serv     TYPE komv,
      pisrate               TYPE mty_taxrate,
      pisbase               TYPE mty_taxrate,
      pispauta              TYPE komv,
      subtribfcpbase1       TYPE mty_taxrate,               "2443042
      subtribfcpbase2       TYPE mty_taxrate,               "2443042
      subtribsurcharge      TYPE mty_taxrate,
      subtribsurtype        TYPE j_1btxstt,
      subtribbase1          TYPE mty_taxrate,
      subtribbase2          TYPE mty_taxrate,
      subtribicms           TYPE mty_taxrate,
      subtribfixprice       TYPE komv,
      subtribminprice       TYPE komv,
      subtribminfactor      TYPE mty_taxrate,
      subtribsurchin        TYPE xfeld,
      icmsnoncontributor    TYPE mty_taxrate,               "2232757
      zonafranca            TYPE xfeld,
      zonafranca_in         TYPE xfeld,                     "1826651
      pautabase             TYPE komv,                      "1290153
      cofinspautarate4dec   TYPE j_1bamount4dec,            "1675010
      pispautarate4dec      TYPE j_1bamount4dec,            "1675010
      ipiminprice           TYPE komv,                      "2141500
      pisminprice           TYPE komv,                      "2141500
      cofinsminprice        TYPE komv,                      "2141500
    END OF mty_tax_data .
  types:
    BEGIN OF mty_tax_result,
      icms_amt                    TYPE mty_taxamount,  "ICMS amount
      icms_dsc100                 TYPE mty_taxamount,  "ICMS discount Convênio 100
      icop_amt                    TYPE mty_taxamount,  "ICMS complement amount
      ipi_amt                     TYPE mty_taxamount,  "IPI amount
      ipio_amt                    TYPE mty_taxamount,  "IPI offset in IPI split vendor
      st_amt                      TYPE mty_taxamount,  "Sub.Trib. amount
      icfr_amt                    TYPE mty_taxamount,  "ICMS on freight amount
      icfs_amt                    TYPE mty_taxamount,  "Sub.Trib. on freight amount
      conh_icm_bas                TYPE mty_taxamount,  "ICMS Conhe Base       "1677119
      conh_icm_oth                TYPE mty_taxamount,  "ICMS Conhe Other Base "1677119
      conh_icm_exc                TYPE mty_taxamount,  "ICMS Conhe Excluded Bs"1714749
      conh_icm_amt                TYPE mty_taxamount,  "ICMS Conhe Amount     "1677119
      conh_st_bas                 TYPE mty_taxamount,  "Sub.Trib. Conhe Base  "1677119
      conh_st_amt                 TYPE mty_taxamount,  "Sub.Trib. Conhe Amnt  "1677119
      conh_st_exc                 TYPE mty_taxamount,  "S.Trib.Conh Excl Base "1714749
      iss_amt                     TYPE mty_taxamount,  "ISS amount
      icms_cbas                   TYPE mty_taxamount,  "ICMS calculation base
      icms_bas                    TYPE mty_taxamount,  "ICMS normal base
      icms_exc                    TYPE mty_taxamount,  "ICMS exclude base
      icms_oth                    TYPE mty_taxamount,  "ICMS other base
      icms_rate                   TYPE mty_taxamount,  "ICMS rate (norm./freight/S.T.)
      icms_fcp_amt                TYPE mty_taxamount,  "ICMS Special Fund Amount                      "2443042
      icms_fcp_obas               TYPE mty_taxamount,  "ICMS Special Fund Other Base                  "2443042
      icms_fcp_base               TYPE mty_taxamount,  "ICMS Special Fund Base                        "2443042 2232757
      icms_fcp_ebas               TYPE mty_taxamount,  "ICMS Special Fund Excluded Base               "2443042 2283474
      icms_fcp_rate               TYPE mty_taxamount,  "ICMS Special Fund Rate                        "2443042 2232757
      icms_fcp_partilha_base      TYPE mty_taxamount, "ICMS Partilha FCP Base               "2443042
      icms_fcp_partilha_ebas      TYPE mty_taxamount, "ICMS Partilha FCP Excluded Base      "2443042
      icms_fcp_partilha_amt       TYPE mty_taxamount,  "ICMS Special Fund Amount Contributor  "2443042 2232757
      icms_orig_part_amt          TYPE mty_taxamount,  "ICMS Partilha Intersate Amount        "2232757
      icms_dest_part_amt          TYPE mty_taxamount,  "ICMS Partilha Intrastate Amount       "2232757
      icms_orig_part_rate         TYPE mty_taxamount,  "ICMS Partilha Intersate Rate          "2232757
      icms_dest_part_rate         TYPE mty_taxamount,  "ICMS Partilha Intrastate Rate         "2232757
      icms_orig_part_base         TYPE mty_taxamount,  "ICMS Partilha Intersate Base          "2232757
      icms_dest_part_base         TYPE mty_taxamount,  "ICMS Partilha Intrastate Base         "2232757
      icms_orig_part_exc          TYPE mty_taxamount,   "ICMS Partilha Intrastate Excl Base    "2273938
      icms_dest_part_exc          TYPE mty_taxamount,   "ICMS Partilha Intrastate Excl Base    "2273938
      ipi_cbas                    TYPE mty_taxamount,  "IPI calculation base
      ipi_bas                     TYPE mty_taxamount,  "IPI normal base
      ipi_exc                     TYPE mty_taxamount,  "IPI exclude base
      ipi_oth                     TYPE mty_taxamount,  "IPI other base
      ipio_bas                    TYPE mty_taxamount,  "IPI base in IPI split vendor
      icop_bas                    TYPE mty_taxamount,  "ICMS complement base
      icop_rate                   TYPE mty_taxamount,  "ICMS complement rate
      icms_comp_fcp_amt           TYPE mty_taxamount, "ICMS Complement FCP Amount              "2443042
      icms_comp_fcp_base          TYPE mty_taxamount, "ICMS Complement FCP Base                "2443042
      icms_comp_fcp_rate          TYPE mty_taxamount, "ICMS Complement FCP Rate                "2443042
      st_bas                      TYPE mty_taxamount,  "Sub.Trib. base
      st_rate                     TYPE mty_taxamount,  "Sub.Trib. rate on nota fiscal
      subtrib_fcp_amt             TYPE mty_taxamount,  "Sub.Trib. FCP rate on nota fiscal       "2443042
      subtrib_fcp_rate            TYPE mty_taxamount,  "Sub.Trib. FCP rate on nota fiscal       "2443042
      subtrib_fcp_base            TYPE mty_taxamount,  "Sub.Trib. FCP base on nota fiscal       "2443042
      icfr_bas                    TYPE mty_taxamount,  "ICMS on freight base
      icfs_bas                    TYPE mty_taxamount,  "Sub.Trib. on freight base
      iss_bas                     TYPE mty_taxamount,  "ISS base
      iss_amt_prov                TYPE mty_taxamount,  "ISS amount at loc. of provider
      iss_wta_prov                TYPE mty_taxamount,  "ISS WT amount at provider
      iss_bas_prov                TYPE mty_taxamount,  "ISS normal base at provider
      iss_exc_prov                TYPE mty_taxamount,  "ISS exclude base at provider
      iss_wt_prov                 TYPE xfeld,          "ISS indicator WT at provider
      iss_amt_serv                TYPE mty_taxamount,  "ISS amount at loc. of service
      iss_wta_serv                TYPE mty_taxamount,  "ISS WT amount at service
      iss_bas_serv                TYPE mty_taxamount,  "ISS normal base at service
      iss_exc_serv                TYPE mty_taxamount,  "ISS exclude base at service
      iss_wt_serv                 TYPE xfeld,          "ISS indicator WT at service
      iss_offset                  TYPE mty_taxamount,  "ISS offsets due to tax included
      iss_offset_service_location TYPE mty_taxamount, "ISS Offset for Service Location "2385097
      iss_offset_provider         TYPE mty_taxamount, "ISS Offset for Provider         "2385097
      cofins_amt                  TYPE mty_taxamount,  "COFINS amount
      cofins_amt_res              TYPE mty_taxamount,  "COFINS amount resale"947218
      cofins_bas                  TYPE mty_taxamount,  "COFINS normal base
      cofins_exc                  TYPE mty_taxamount,  "COFINS exclude base
      cofins_off                  TYPE mty_taxamount,  "COFINS offset tax included
      pis_amt                     TYPE mty_taxamount,  "PIS amount
      pis_amt_res                 TYPE mty_taxamount,  "PIS amount resale "947218
      pis_bas                     TYPE mty_taxamount,  "PIS normal base
      pis_exc                     TYPE mty_taxamount,  "PIS exclude base
      pis_off                     TYPE mty_taxamount,  "PIS offset due to tax included
      piscof_config_base2         TYPE mty_taxamount, "configured PIS/COFINS base for    "1717837
      "2. base calculation               "1717837
      ipi_pauta_base              TYPE mty_taxamount,       "1818634
      pis_pauta_base              TYPE mty_taxamount,       "1818634
      cofins_pauta_base           TYPE mty_taxamount,       "1818634
    END OF mty_tax_result .

  methods CALCULATE_BASE_PISCOF
    importing
      !IS_TAXDATA type MTY_TAX_DATA
      !IS_TAXRESULT type MTY_TAX_RESULT
      !IS_KOMK type KOMK optional
      !IS_KOMP type KOMP
    exporting
      value(EV_BASE) type MTY_BASE .
  methods CHECK_CALC_ACTIVE
    importing
      !IS_KOMK type KOMK
      !IS_KOMP type KOMP
    exporting
      !EV_CALC_ACTIVE type FLAG .
endinterface.
