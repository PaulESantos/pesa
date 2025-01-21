#' Setup Fonts for Social Media Captions
#'
#' Configures and loads the necessary fonts for rendering social media icons and text.
#'
#' @param fontfamily The name of the font to be used for the text. This font is loaded using Google Fonts.
#' @return No return value; the function sets up the fonts for rendering text and icons.
#' @export
setup_fonts <- function(fontfamily) {
  # Load Font Awesome Brands
  sysfonts::font_add(family = "fa6-brands", regular = "inst/extdata/fa-brands-400.ttf")
  # Load additional text font
  sysfonts::font_add_google(name = fontfamily, family = "fontfamily")
  # Enable automatic text rendering
  showtext::showtext_auto(enable = TRUE)
}

#' Get Social Media Icons
#'
#' Provides a list of HTML strings representing social media icons using Font Awesome 6.
#'
#' @return A named list containing the HTML code for each social media icon (LinkedIn, GitHub, Twitter, and Bluesky).
#' @examples
#' icons <- get_social_icons()
#' icons$twitter # Returns the HTML for the Twitter icon
#' @export
get_social_icons <- function() {
  list(
    linkedin = "<span style='font-family:\"fa6-brands\";'>&#xf08c;</span>",  # LinkedIn icon
    github   = "<span style='font-family:\"fa6-brands\";'>&#xf09b;</span>",  # GitHub icon
    twitter  = "<span style='font-family:\"fa6-brands\";'>&#xe61b;</span>",  # Twitter icon
    bluesky  = "<span style='font-family:\"fa6-brands\";'>&#xe6a3;</span>"   # Bluesky icon
  )
}

#' Create Social Media Caption
#'
#' Generates an HTML string with styled icons and usernames for various social media platforms.
#'
#' @param usernames A named list with usernames for the social media platforms: "twitter", "github", "linkedin", and "bluesky".
#' @param highlight_color The color for the icons (default: "#fb0f01").
#' @param text_color The color for the text (default: "#2F4F4F").
#' @param fontfamily The font family to use for the usernames, loaded via Google Fonts.
#' @return An HTML string representing the styled social media caption.
#' @examples
#' usernames <- list(
#'   twitter = "example_user",
#'   github = "example_repo",
#'   linkedin = "example_profile",
#'   bluesky = "example_bluesky"
#' )
#' p_social_caption(usernames, fontfamily = "Commissioner")
#' @export
p_social_caption <- function(usernames,
                                  highlight_color = "#fb0f01",
                                  text_color = "#2F4F4F",
                                  fontfamily) {
  # Ensure fonts are configured
  setup_fonts(fontfamily)

  # Get social media icons
  icons <- get_social_icons()

  # Construct the social media caption
  social_text <- stringr::str_glue(
    "<span style='font-family:\"fa6-brands\";color:{highlight_color};'>{icons$twitter}</span>
    <span style='font-family:fontfamily;color:{text_color};'>@{usernames$twitter}</span>
    <span style='color:white;'>..</span>
    <span style='font-family:\"fa6-brands\";color:{highlight_color};'>{icons$github}</span>
    <span style='font-family:fontfamily;color:{text_color};'>{usernames$github}</span>
    <span style='color:white;'>..</span>
    <span style='font-family:\"fa6-brands\";color:{highlight_color};'>{icons$linkedin}</span>
    <span style='font-family:fontfamily;color:{text_color};'>{usernames$linkedin}</span>
    <span style='color:white;'>..</span>
    <span style='font-family:\"fa6-brands\";color:{highlight_color};'>{icons$bluesky}</span>
    <span style='font-family:fontfamily;color:{text_color};'>{usernames$bluesky}</span>"
  )

  # Return the HTML caption
  social_text
}
