# The 32 items of COREQ, verbatim from Table 1 of Tong, Sainsbury and Craig
# (2007) doi:10.1093/intqhc/mzm042. Reproduced as published: a checklist
# paraphrased is a checklist a reviewer cannot match against the original.
COREQ_ITEMS <- list(
  c("1", "Research team and reflexivity", "Personal characteristics", "Interviewer/facilitator",
    "Which author/s conducted the interview or focus group?"),
  c("2", "Research team and reflexivity", "Personal characteristics", "Credentials",
    "What were the researcher's credentials? E.g. PhD, MD"),
  c("3", "Research team and reflexivity", "Personal characteristics", "Occupation",
    "What was their occupation at the time of the study?"),
  c("4", "Research team and reflexivity", "Personal characteristics", "Gender",
    "Was the researcher male or female?"),
  c("5", "Research team and reflexivity", "Personal characteristics", "Experience and training",
    "What experience or training did the researcher have?"),
  c("6", "Research team and reflexivity", "Relationship with participants", "Relationship established",
    "Was a relationship established prior to study commencement?"),
  c("7", "Research team and reflexivity", "Relationship with participants", "Participant knowledge of the interviewer",
    "What did the participants know about the researcher? e.g. personal goals, reasons for doing the research"),
  c("8", "Research team and reflexivity", "Relationship with participants", "Interviewer characteristics",
    "What characteristics were reported about the interviewer/facilitator? e.g. Bias, assumptions, reasons and interests in the research topic"),
  c("9", "Study design", "Theoretical framework", "Methodological orientation and Theory",
    "What methodological orientation was stated to underpin the study? e.g. grounded theory, discourse analysis, ethnography, phenomenology, content analysis"),
  c("10", "Study design", "Participant selection", "Sampling",
    "How were participants selected? e.g. purposive, convenience, consecutive, snowball"),
  c("11", "Study design", "Participant selection", "Method of approach",
    "How were participants approached? e.g. face-to-face, telephone, mail, email"),
  c("12", "Study design", "Participant selection", "Sample size",
    "How many participants were in the study?"),
  c("13", "Study design", "Participant selection", "Non-participation",
    "How many people refused to participate or dropped out? Reasons?"),
  c("14", "Study design", "Setting", "Setting of data collection",
    "Where was the data collected? e.g. home, clinic, workplace"),
  c("15", "Study design", "Setting", "Presence of non-participants",
    "Was anyone else present besides the participants and researchers?"),
  c("16", "Study design", "Setting", "Description of sample",
    "What are the important characteristics of the sample? e.g. demographic data, date"),
  c("17", "Study design", "Data collection", "Interview guide",
    "Were questions, prompts, guides provided by the authors? Was it pilot tested?"),
  c("18", "Study design", "Data collection", "Repeat interviews",
    "Were repeat interviews carried out? If yes, how many?"),
  c("19", "Study design", "Data collection", "Audio/visual recording",
    "Did the research use audio or visual recording to collect the data?"),
  c("20", "Study design", "Data collection", "Field notes",
    "Were field notes made during and/or after the interview or focus group?"),
  c("21", "Study design", "Data collection", "Duration",
    "What was the duration of the interviews or focus group?"),
  c("22", "Study design", "Data collection", "Data saturation",
    "Was data saturation discussed?"),
  c("23", "Study design", "Data collection", "Transcripts returned",
    "Were transcripts returned to participants for comment and/or correction?"),
  c("24", "Analysis and findings", "Data analysis", "Number of data coders",
    "How many data coders coded the data?"),
  c("25", "Analysis and findings", "Data analysis", "Description of the coding tree",
    "Did authors provide a description of the coding tree?"),
  c("26", "Analysis and findings", "Data analysis", "Derivation of themes",
    "Were themes identified in advance or derived from the data?"),
  c("27", "Analysis and findings", "Data analysis", "Software",
    "What software, if applicable, was used to manage the data?"),
  c("28", "Analysis and findings", "Data analysis", "Participant checking",
    "Did participants provide feedback on the findings?"),
  c("29", "Analysis and findings", "Reporting", "Quotations presented",
    "Were participant quotations presented to illustrate the themes / findings? Was each quotation identified? e.g. participant number"),
  c("30", "Analysis and findings", "Reporting", "Data and findings consistent",
    "Was there consistency between the data presented and the findings?"),
  c("31", "Analysis and findings", "Reporting", "Clarity of major themes",
    "Were major themes clearly presented in the findings?"),
  c("32", "Analysis and findings", "Reporting", "Clarity of minor themes",
    "Is there a description of diverse cases or discussion of minor themes?"))

#' A COREQ checklist, pre-filled with what the data can answer
#'
#' COREQ (Tong, Sainsbury & Craig 2007, \doi{10.1093/intqhc/mzm042}) is a
#' submission requirement at many journals: 32 items across research team,
#' study design, and analysis and findings.  Most of them only the researcher
#' can answer.  Six of them the exports already know, and filling those in
#' saves the tedious part while making the rest visible as gaps.
#'
#' The point is not automation.  It is that the numbers a reviewer will ask
#' for -- how many documents, how many coders, how large the code system, was
#' saturation discussed -- come out of the data rather than out of memory, and
#' therefore match what the analysis actually did.
#'
#' @section What COREQ does not ask:
#' There is no item for intercoder agreement.  If you computed it, it belongs
#' in your answer to item 24 or 25; the checklist will not prompt you.
#'
#' @param fragments A fragments data frame, or `NULL`.
#' @param history A history data frame, or `NULL`; enables the saturation item.
#' @param codebook A codebook data frame, or `NULL`; enables the coding-tree item.
#' @param software Free text for item 27.  The default names the tools in use.
#' @return A data frame with one row per item: `item`, `domain`, `section`,
#'   `name`, `question`, `answer` and `filled` (`TRUE` where the data supplied
#'   it).
#' @examples
#' frag <- qda_read_fragments(qda_example("zotqda-fragments.csv"))
#' cq <- qda_coreq(frag)
#' cq[cq$filled, c("item", "name", "answer")]
#' @export
qda_coreq <- function(fragments = NULL, history = NULL, codebook = NULL,
                      software = NULL) {
  answers <- list()
  if (!is.null(fragments)) {
    docs <- unique(as.character(fragments$citekey[nzchar(fragments$citekey)]))
    if (!length(docs)) docs <- unique(as.character(fragments$title))
    answers[["12"]] <- paste0(length(docs), " documents in the export")
    coders <- unique(as.character(fragments$codedBy[nzchar(fragments$codedBy)]))
    if (length(coders)) {
      answers[["24"]] <- paste0(length(coders), " coder(s): ",
                                paste(sort(coders), collapse = ", "))
    }
    quoted <- sum(nzchar(as.character(fragments$text)))
    answers[["29"]] <- paste0(quoted, " coded fragments are available as ",
                              "quotations, each with its annotation key")
  }
  if (!is.null(history)) {
    sat <- qda_saturation_ratio(qda_new_codes(history))
    answers[["22"]] <- if (is.null(sat$notation)) {
      paste0("code saturation not reached (", sat$reason, ")")
    } else {
      paste0("code saturation at ", sat$notation,
             " (Guest, Namey and Chen 2020; base ", sat$base_size,
             ", threshold ", sat$threshold * 100, " per cent)")
    }
  }
  if (!is.null(codebook)) {
    levels <- suppressWarnings(as.numeric(codebook$level))
    depth <- if (any(is.finite(levels))) max(levels, na.rm = TRUE) else 1
    answers[["25"]] <- paste0(nrow(codebook), " codes over ", max(1, depth),
                              " levels; export the code system for the guide")
  }
  answers[["27"]] <- if (is.null(software)) {
    paste0("zotQDA (Zotero plugin) for coding; qdaR ",
           utils::packageVersion("qdaR"), " for analysis")
  } else software

  do.call(rbind, lapply(COREQ_ITEMS, function(it) {
    given <- answers[[it[1]]]
    data.frame(item = as.integer(it[1]), domain = it[2], section = it[3],
               name = it[4], question = it[5],
               answer = if (is.null(given)) "" else given,
               filled = !is.null(given), stringsAsFactors = FALSE)
  }))
}

#' The checklist as Markdown, ready to paste into a submission
#'
#' @param coreq A data frame from [qda_coreq()].
#' @param title Heading for the document.
#' @param file Optional path to write to.
#' @return A character vector of Markdown lines.
#' @examples
#' frag <- qda_read_fragments(qda_example("zotqda-fragments.csv"))
#' head(qda_coreq_markdown(qda_coreq(frag)), 8)
#' @export
qda_coreq_markdown <- function(coreq, title = "COREQ checklist", file = NULL) {
  out <- c(paste("#", title), "",
           paste0("Consolidated criteria for reporting qualitative research ",
                  "(Tong, Sainsbury and Craig 2007, ",
                  "doi:10.1093/intqhc/mzm042). Answers marked *from the data* ",
                  "were derived from the exports; the rest are for you to ",
                  "complete."),
           "")
  for (domain in unique(coreq$domain)) {
    out <- c(out, paste("##", domain), "")
    part <- coreq[coreq$domain == domain, , drop = FALSE]
    for (section in unique(part$section)) {
      out <- c(out, paste("###", section), "")
      rows <- part[part$section == section, , drop = FALSE]
      for (i in seq_len(nrow(rows))) {
        r <- rows[i, ]
        out <- c(out, paste0("**", r$item, ". ", r$name, "** -- ", r$question))
        out <- c(out, if (nzchar(r$answer)) {
          paste0("*From the data:* ", r$answer)
        } else "*To be completed.*", "")
      }
    }
  }
  if (!is.null(file)) writeLines(out, file)
  out
}

# The 21 standards of SRQR, verbatim from Table 1 of O'Brien, Harris,
# Beckman, Reed and Cook (2014) doi:10.1097/ACM.0000000000000388.
SRQR_ITEMS <- list(
  c("S1", "Title and abstract", "Title",
    "Concise description of the nature and topic of the study Identifying the study as qualitative or indicating the approach (e.g., ethnography, grounded theory) or data collection methods (e.g., interview, focus group) is recommended"),
  c("S2", "Title and abstract", "Abstract",
    "Summary of key elements of the study using the abstract format of the intended publication; typically includes background, purpose, methods, results, and conclusions"),
  c("S3", "Introduction", "Problem formulation",
    "Description and significance of the problem/phenomenon studied; review of relevant theory and empirical work; problem statement"),
  c("S4", "Introduction", "Purpose or research question",
    "Purpose of the study and specific objectives or questions"),
  c("S5", "Methods", "Qualitative approach and research paradigm",
    "Qualitative approach (e.g., ethnography, grounded theory, case study, phenomenology, narrative research) and guiding theory if appropriate; identifying the research paradigm (e.g., postpositivist, constructivist/interpretivist) is also recommended; rationale"),
  c("S6", "Methods", "Researcher characteristics and reflexivity",
    "Researchers' characteristics that may influence the research, including personal attributes, qualifications/experience, relationship with participants, assumptions, and/or presuppositions; potential or actual interaction between researchers' characteristics and the research questions, approach, methods, results, and/or transferability"),
  c("S7", "Methods", "Context",
    "Setting/site and salient contextual factors; rationale"),
  c("S8", "Methods", "Sampling strategy",
    "How and why research participants, documents, or events were selected; criteria for deciding when no further sampling was necessary (e.g., sampling saturation); rationale"),
  c("S9", "Methods", "Ethical issues pertaining to human subjects",
    "Documentation of approval by an appropriate ethics review board and participant consent, or explanation for lack thereof; other confidentiality and data security issues"),
  c("S10", "Methods", "Data collection methods",
    "Types of data collected; details of data collection procedures including (as appropriate) start and stop dates of data collection and analysis, iterative process, triangulation of sources/methods, and modification of procedures in response to evolving study findings; rationale"),
  c("S11", "Methods", "Data collection instruments and technologies",
    "Description of instruments (e.g., interview guides, questionnaires) and devices (e.g., audio recorders) used for data collection; if/how the instrument(s) changed over the course of the study"),
  c("S12", "Methods", "Units of study",
    "Number and relevant characteristics of participants, documents, or events included in the study; level of participation (could be reported in results)"),
  c("S13", "Methods", "Data processing",
    "Methods for processing data prior to and during analysis, including transcription, data entry, data management and security, verification of data integrity, data coding, and anonymization/deidentification of excerpts"),
  c("S14", "Methods", "Data analysis",
    "Process by which inferences, themes, etc., were identified and developed, including the researchers involved in data analysis; usually references a specific paradigm or approach; rationale"),
  c("S15", "Methods", "Techniques to enhance trustworthiness",
    "Techniques to enhance trustworthiness and credibility of data analysis (e.g., member checking, audit trail, triangulation); rationale"),
  c("S16", "Results/findings", "Synthesis and interpretation",
    "Main findings (e.g., interpretations, inferences, and themes); might include development of a theory or model, or integration with prior research or theory"),
  c("S17", "Results/findings", "Links to empirical data",
    "Evidence (e.g., quotes, field notes, text excerpts, photographs) to substantiate analytic findings"),
  c("S18", "Discussion", "Integration with prior work, implications, transferability, and contribution(s) to the field",
    "Short summary of main findings; explanation of how findings and conclusions connect to, support, elaborate on, or challenge conclusions of earlier scholarship; discussion of scope of application/generalizability; identification of unique contribution(s) to scholarship in a discipline or field"),
  c("S19", "Discussion", "Limitations",
    "Trustworthiness and limitations of findings"),
  c("S20", "Other", "Conflicts of interest",
    "Potential sources of influence or perceived influence on study conduct and conclusions; how these were managed"),
  c("S21", "Other", "Funding",
    "Sources of funding and other support; role of funders in data collection, interpretation, and reporting"))

#' An SRQR checklist, pre-filled with what the data can answer
#'
#' SRQR (O'Brien et al. 2014, \doi{10.1097/ACM.0000000000000388}) is the other
#' reporting standard journals ask for, and it is the broader of the two: 21
#' standards covering the whole report rather than COREQ's focus on interviews
#' and focus groups.  Use it when your material is not interview transcripts,
#' or when the journal names it.
#'
#' @section Where the agreement figure belongs:
#' Unlike COREQ, SRQR has a home for it.  Standard S15, techniques to enhance
#' trustworthiness, names the audit trail explicitly -- and the coding log is
#' one.  If you computed intercoder agreement, that is the item it answers.
#' [qda_coreq()] has to say the opposite, because COREQ never asks.
#'
#' @inheritParams qda_coreq
#' @return A data frame with one row per standard: `item`, `section`, `name`,
#'   `description`, `answer` and `filled`.
#' @examples
#' frag <- qda_read_fragments(qda_example("zotqda-fragments.csv"))
#' sq <- qda_srqr(frag)
#' sq[sq$filled, c("item", "name")]
#' @export
qda_srqr <- function(fragments = NULL, history = NULL, codebook = NULL,
                     software = NULL) {
  answers <- list()
  if (!is.null(fragments)) {
    docs <- unique(as.character(fragments$citekey[nzchar(fragments$citekey)]))
    if (!length(docs)) docs <- unique(as.character(fragments$title))
    answers[["S12"]] <- paste0(length(docs), " documents, ",
                               length(unique(fragments$annotationKey)),
                               " coded segments")
    coders <- unique(as.character(fragments$codedBy[nzchar(fragments$codedBy)]))
    if (length(coders)) {
      answers[["S14"]] <- paste0(length(coders), " coder(s) involved: ",
                                 paste(sort(coders), collapse = ", "))
    }
    answers[["S17"]] <- paste0(sum(nzchar(as.character(fragments$text))),
                               " coded fragments are available as evidence, ",
                               "each identified by its annotation key")
  }
  if (!is.null(history)) {
    sat <- qda_saturation_ratio(qda_new_codes(history))
    answers[["S8"]] <- if (is.null(sat$notation)) {
      paste0("code saturation not reached (", sat$reason,
             "); state your own stopping criterion")
    } else {
      paste0("code saturation at ", sat$notation,
             " (Guest, Namey and Chen 2020) -- note this is code saturation, ",
             "not meaning saturation")
    }
    answers[["S15"]] <- paste0("audit trail: the coding log records ",
                               nrow(history), " events with coder and time; ",
                               "add your intercoder agreement here")
  }
  answers[["S13"]] <- paste0("coding in zotQDA (Zotero plugin); ",
                             if (is.null(codebook)) "" else
                               paste0(nrow(codebook), " codes; "),
                             "analysis in qdaR ", utils::packageVersion("qdaR"))
  if (!is.null(software)) answers[["S13"]] <- software

  do.call(rbind, lapply(SRQR_ITEMS, function(it) {
    given <- answers[[it[1]]]
    data.frame(item = it[1], section = it[2], name = it[3],
               description = it[4],
               answer = if (is.null(given)) "" else given,
               filled = !is.null(given), stringsAsFactors = FALSE)
  }))
}

#' The SRQR checklist as Markdown
#'
#' @param srqr A data frame from [qda_srqr()].
#' @param title Heading for the document.
#' @param file Optional path to write to.
#' @return A character vector of Markdown lines.
#' @examples
#' frag <- qda_read_fragments(qda_example("zotqda-fragments.csv"))
#' head(qda_srqr_markdown(qda_srqr(frag)), 6)
#' @export
qda_srqr_markdown <- function(srqr, title = "SRQR checklist", file = NULL) {
  out <- c(paste("#", title), "",
           paste0("Standards for Reporting Qualitative Research (O'Brien, ",
                  "Harris, Beckman, Reed and Cook 2014, ",
                  "doi:10.1097/ACM.0000000000000388). Answers marked *from ",
                  "the data* were derived from the exports; the rest are for ",
                  "you to complete."),
           "")
  for (section in unique(srqr$section)) {
    out <- c(out, paste("##", section), "")
    rows <- srqr[srqr$section == section, , drop = FALSE]
    for (i in seq_len(nrow(rows))) {
      r <- rows[i, ]
      out <- c(out, paste0("**", r$item, " ", r$name, "** -- ", r$description))
      out <- c(out, if (nzchar(r$answer)) {
        paste0("*From the data:* ", r$answer)
      } else "*To be completed.*", "")
    }
  }
  if (!is.null(file)) writeLines(out, file)
  out
}
