body <- bs4DashBody(
  includeCSS("https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Styles/style.css"),
  includeScript("https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Scripts/scripts"),
  use_waiter(),
  useShinyjs(),
  bs4TabItems(
    bs4TabItem(tabName = "TablaGeneral"),
    bs4TabItem(tabName = "SerieTiempo")
    )
  )
