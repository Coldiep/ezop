
#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string.hpp>

#include <Wt/WString>
#include <Wt/WAnchor>
#include <Wt/WApplication>
#include <Wt/WCheckBox>
#include <Wt/WContainerWidget>
#include <Wt/WEnvironment>
#include <Wt/WLineEdit>
#include <Wt/WPushButton>
#include <Wt/WStackedWidget>
#include <Wt/WTemplate>
#include <Wt/WText>

#include <Wt/Dbo/backend/Sqlite3>

#include <web/site/model/project.h>
#include <web/site/model/ontology.h>
#include <web/site/model/user.h>
#include <web/site/site_session.h>

#include <web/site/view/morda.h>
using ezop::web::Morda;

namespace ezop { namespace web {

class Site : public Wt::WContainerWidget {
public:
  Site(const std::string& base_path, const std::string& sqlite_db, ezop::web::Morda* morda)
    : base_path_(base_path)
    , session_(sqlite_db)
    , morda_(morda)
    , panel_(NULL)
    , register_(NULL)
    , profile_(NULL)
    , author_panel_(NULL)
    //, users_(NULL)
    //, user_editor_(NULL)
    , must_login_warning_(NULL)
    , must_be_administrator_warning_(NULL)
    , invalid_user_(NULL) {

    Wt::WApplication* app = wApp;
    app->messageResourceBundle().use(Wt::WApplication::appRoot() + "site");
    app->useStyleSheet("/css/blog.css");
    //app->internalPathChanged().connect(this, &Site::HandlePathChange);
    login_ = new Wt::WTemplate(this);
    panel_ = new Wt::WStackedWidget(this);
    items_ = new Wt::WContainerWidget(this);

    //Init();

    std::string token;
    try {
      token = app->environment().getCookie("ezoplogin");
    } catch (...) {
    }

    if (not token.empty()) {
      Wt::Dbo::Transaction t(session_);
      Wt::Dbo::ptr<ezop::web::User> user = session_.find<ezop::web::User>("where token = ?").bind(token);
      if (user) {
        //LoginAs(user);
      }
      t.commit();
    }
  }

  ezop::web::SiteSession& Session() {
    return session_;
  }

  ~Site() {
    //Clear();
  }

  std::string       base_path_;
  ezop::web::SiteSession       session_;
  ezop::web::Morda*            morda_;

  Wt::WTemplate*        login_;
  Wt::WStackedWidget*   panel_;
  Wt::WTemplate*        register_;
  Wt::WTemplate*        profile_;
  Wt::WTemplate*        author_panel_;
  //EditUsers*        users_;
  //EditUser *        user_editor_;
  Wt::WTemplate*        must_login_warning_;
  Wt::WTemplate*        must_be_administrator_warning_;
  Wt::WTemplate*        invalid_user_;
  Wt::WContainerWidget* items_;
};
}} // namespace ezop, web.
#if 0
  void Logout() {
    if (boost::starts_with(wApp->internalPath(), basePath_ + "author/")) {
      wApp->setInternalPath(basePath_, true);
    }

    wApp->setCookie("sitelogin", "", 0);
    morda_->UserChanged().emit(WString::Empty);

    Init();
  }

  void Init() {
    session_.SetUser(Wt::Dbo::ptr<User>());
    refresh();
    panel_->hide();

    login_->clear();
    login_->setTemplateText(tr("ezop-login"));

    WCheckBox* remember_me = new WCheckBox(tr("remember-me"));
    WLineEdit* name = new WLineEdit();
    name->setEmptyText("login");
    name->setToolTip("login");
    WLineEdit* passwd = new WLineEdit();
    passwd->setEmptyText("password");
    passwd->setToolTip("password");
    WPushButton* login_button = new WPushButton(tr("login"));

    passwd->setEchoMode(WLineEdit::Password);
    passwd->enterPressed().connect(this, &Site::Login);
    loginButton->clicked().connect(this, &Site::Login);

    remember_me->hide();
    name->hide();
    passwd->hide();
    loginButton->hide();

    WText* login_link = new WText(tr("login"));
    login_link->setStyleClass("link");

    login_link->clicked().connect(remember_me, &WWidget::show);
    login_link->clicked().connect(name, &WWidget::show);
    login_link->clicked().connect(passwd, &WWidget::show);
    login_link->clicked().connect(loginButton, &WWidget::show);
    login_link->clicked().connect(loginLink, &WWidget::hide);
    login_link->clicked().connect(name, &WFormWidget::setFocus);

    WText* register_link = new WText(tr("register"));
    register_link->setStyleClass("link");
    register_link->clicked().connect(this, &Site::NewUser);

    login_->bindWidget("rememberMe", rememberMe);
    login_->bindWidget("name", name);
    login_->bindWidget("passwd", passwd);
    login_->bindWidget("login-button", loginButton);
    login_->bindWidget("login-link", loginLink);
    login_->bindWidget("register-link", registerLink);
    login_->bindString("feed-url", rssFeedUrl_);

    login_->bindWidget("archive-link", createArchiveLink());
  }

  void Login() {
    WLineEdit* name = login_->resolve<WLineEdit*>("name");
    WLineEdit* passwd = login_->resolve<WLineEdit*>("passwd");
    WCheckBox* rememberMe = login_->resolve<WCheckBox*>("remember_me");

    Wt::Dbo::Transaction t(session_);

    Wt::Dbo::ptr<User> user = session_.find<User>("where name = ?").bind(name->text());

    if (user) {
      if (user->Authenticate(passwd->text().toUTF8())) {
        if (remember_me->isChecked()) {
          std::string token = user.modify()->GenerateToken();
          WApplication* app = wApp;
          app->setCookie("ezoplogin", token, 60*60*24*14);
        }
        LoginAs(user);
      } else {
        name->removeStyleClass("Wt-invalid", true);
        passwd->addStyleClass("Wt-invalid", true);
      }
    } else {
      name->addStyleClass("Wt-invalid", true);
    }

    t.commit();
  }
};

  void LoginAs(Wt::Dbo::ptr<User> user) {
    session_.SetUser(user);

    WApplication::instance()->changeSessionId();
    morda_->UserChanged().emit(user->name_);

    refresh();
    login_->clear();
    login_->setTemplateText(tr("ezop-logout"));

    cancelRegister();

    WText *profileLink = new WText(tr("profile"));
    profileLink->setStyleClass("link");
    profileLink->clicked().connect(this, &BlogImpl::editProfile);

    if (user->role == User::Admin) {
      WText *editUsersLink = new WText(tr("edit-users"));
      editUsersLink->setStyleClass("link");
      editUsersLink->clicked().connect(SLOT(this, BlogImpl::editUsers));
      login_->bindWidget("userlist-link", editUsersLink);
      WText *authorPanelLink = new WText(tr("author-post"));
      authorPanelLink->setStyleClass("link");
      authorPanelLink->clicked().connect(SLOT(this, BlogImpl::authorPanel));
      login_->bindWidget("author-panel-link", authorPanelLink);
    } else {
      login_->bindWidget("userlist-link", new WText(""));
      login_->bindWidget("author-panel-link", new WText(""));
    }
 
    WText *logoutLink = new WText(tr("logout"));
    logoutLink->setStyleClass("link");
    logoutLink->clicked().connect(this, &BlogImpl::logout);

    login_->bindString("feed-url", rssFeedUrl_);
    login_->bindString("user", user->name);
    login_->bindWidget("profile-link", profileLink);
    login_->bindWidget("logout-link", logoutLink);

    login_->bindWidget("archive-link", createArchiveLink());

    bindPanelTemplates();
  }

  void NewUser() {
    if (not register_) {
      register_ = new WTemplate();
      insertWidget(1, register_);
      register_->setTemplateText(tr("blog-register"));

      WLineEdit* name = new WLineEdit();
      WLineEdit* passwd = new WLineEdit();
      WLineEdit* passwd2 = new WLineEdit();
      WPushButton* ok_button = new WPushButton(tr("register"));
      WPushButton* cancel_button = new WPushButton(tr("cancel"));
      WText* error = new WText();

      passwd->setEchoMode(WLineEdit::Password);
      passwd2->setEchoMode(WLineEdit::Password);

      ok_button->clicked().connect(this, &Site::doRegister);
      cancel_button->clicked().connect(this, &Site::cancelRegister);

      register_->bindWidget("name", name);
      register_->bindWidget("passwd", passwd);
      register_->bindWidget("passwd2", passwd2);
      register_->bindWidget("ok-button", ok_button);
      register_->bindWidget("cancel-button", cancel_button);
      register_->bindWidget("error", error);
    }
  }

  void bindPanelTemplates() {
    if (!session_.user()) return;
    Wt::Wt::Dbo::Transaction t(session_);
    if (authorPanel_) {
      WPushButton *newPost = new WPushButton(tr("new-post"));
      newPost->clicked().connect(SLOT(this, BlogImpl::newPost));
      WContainerWidget *unpublishedPosts = new WContainerWidget();
      showPosts(session_.user()->allPosts(Post::Unpublished), unpublishedPosts);

      authorPanel_->bindString("user", session_.user()->name);
      authorPanel_->bindInt("unpublished-count",
			    (int)session_.user()->allPosts(Post::Unpublished).size());
      authorPanel_->bindInt("published-count",
			    (int)session_.user()->allPosts(Post::Published).size());
      authorPanel_->bindWidget("new-post", newPost);
      authorPanel_->bindWidget("unpublished-posts", unpublishedPosts);
    }
    if (profile_)
      profile_->bindString("user",session_.user()->name);
  }
 
  void editUsers() {
    panel_->show();
    if (!users_) {
      users_ = new EditUsers(session_, basePath_);
      panel_->addWidget(users_);
      bindPanelTemplates();
    }
    panel_->setCurrentWidget(users_);
  }

 void authorPanel() {
    panel_->show();
    if (!authorPanel_)
    {
      authorPanel_ = new WTemplate(tr("blog-author-panel"));
      panel_->addWidget(authorPanel_);
      bindPanelTemplates();
    }
    panel_->setCurrentWidget(authorPanel_);
  }

  void editProfile() {
    panel_->show();
    if (!profile_) {
      profile_ = new WTemplate(tr("blog-profile"));
      panel_->addWidget(profile_);
      bindPanelTemplates();

      WLineEdit *passwd = new WLineEdit();
      WLineEdit *passwd2 = new WLineEdit();
      WPushButton *okButton = new WPushButton(tr("save"));
      WPushButton *cancelButton = new WPushButton(tr("cancel"));
      WText *error = new WText();

      passwd->setEchoMode(WLineEdit::Password);
      passwd2->setEchoMode(WLineEdit::Password);
      okButton->clicked().connect(this, &BlogImpl::saveProfile);
      cancelButton->clicked().connect(this, &BlogImpl::cancelProfile);

      profile_->bindWidget("passwd", passwd);
      profile_->bindWidget("passwd2", passwd2);
      profile_->bindWidget("ok-button", okButton);
      profile_->bindWidget("cancel-button", cancelButton);
      profile_->bindWidget("error", error);
    }
    panel_->setCurrentWidget(profile_);
  }

  void cancelProfile() {
    WLineEdit *passwd = profile_->resolve<WLineEdit *>("passwd");
    WLineEdit *passwd2 = profile_->resolve<WLineEdit *>("passwd2");
    WText *error = profile_->resolve<WText *>("error");
    passwd->setText(WString());
    passwd2->setText(WString());
    error->setText(WString());
    panel_->hide();
  }

  void saveProfile() {
    WLineEdit *passwd = profile_->resolve<WLineEdit *>("passwd");
    WLineEdit *passwd2 = profile_->resolve<WLineEdit *>("passwd2");
    WText *error = profile_->resolve<WText *>("error");
    if (passwd->text().empty()) {
      cancelProfile();
      return;
    }
    if (passwd->text() != passwd2->text()) {
      error->setText(tr("passwd-mismatch"));
      return;
    }
    Wt::Dbo::Transaction t(session_);
    session_.user().modify()->setPassword(passwd->text().toUTF8());
    t.commit();
    cancelProfile();
  }

  void doRegister() {
    WLineEdit *name = register_->resolve<WLineEdit *>("name");
    WLineEdit *passwd = register_->resolve<WLineEdit *>("passwd");
    WLineEdit *passwd2 = register_->resolve<WLineEdit *>("passwd2");
    WText *error = register_->resolve<WText *>("error");

    if (passwd->text() != passwd2->text()) {
      error->setText(tr("passwd-mismatch"));
      return;
    }

    Wt::Dbo::Transaction t(session_);

    Wt::Dbo::ptr<User> user
      = session_.find<User>("where name = ?").bind(name->text());

    if (user) {
      error->setText(tr("user-exists").arg(name->text()));
      t.commit();
      return;
    } else {
      std::string n = name->text().toUTF8();
      boost::trim(n);
      if (n.length() < 4) {
	error->setText(tr("login-tooshort").arg(4));
	t.commit();
	return;
      }

      user = session_.add(new User());

      user.modify()->name = n;
      user.modify()->role = User::Visitor;
      user.modify()->setPassword(passwd->text().toUTF8());

      t.commit();

      loginAs(user);
    }
  }

  void cancelRegister() {
    delete register_;
    register_ = 0;
  }

  void refresh() {
    handlePathChange(wApp->internalPath());
  }

  void handlePathChange(const std::string& path) {
    WApplication *app = wApp;

    if (app->internalPathMatches(basePath_)) {
      Wt::Dbo::Transaction t(session_);

      std::string path = app->internalPathNextPart(basePath_);

      items_->clear();
      if (profile_) {
	delete profile_;
	profile_ = 0;
      }

      if (users_) {
	delete users_;
	users_ = 0;
      }

      if (path.empty())
	showPosts(session_.find<Post>
		  ("where state = ? "
		   "order by date desc "
		   "limit 10").bind(Post::Published), items_);

      else if (path == "author") {
	std::string author = app->internalPathNextPart(basePath_ + path + '/');
	Wt::Dbo::ptr<User> user = findUser(author);

	if (user)
	  showPosts(user);
	else
	  showError(tr("blog-no-author").arg(author));
      } else if (path == "edituser") {
	editUser(app->internalPathNextPart(basePath_ + path + '/'));
      } else if (path == "all") {
	showArchive(items_);
      } else {
	std::string remainder = app->internalPath().substr(basePath_.length());
	showPostsByDateTopic(remainder, items_);
      }

      t.commit();
    }
  }

  void editUser(const std::string& ids) {
    if (!checkLoggedIn()) return;
    if (!checkAdministrator()) return;
    Wt::Dbo::dbo_traits<User>::IdType id;
    try {
      id = boost::lexical_cast<Wt::Dbo::dbo_traits<User>::IdType>(ids);
    } catch (boost::bad_lexical_cast&) {
      id = Wt::Dbo::dbo_traits<User>::invalidId();
    }
    panel_->show();
    try {
      Wt::Dbo::Transaction t(session_);
      Wt::Dbo::ptr<User> target(session_.load<User>(id));
      if (!userEditor_)
	panel_->addWidget(userEditor_ = new EditUser(session_));
      userEditor_->switchUser(target);
      panel_->setCurrentWidget(userEditor_);
    }
    catch (Wt::Dbo::ObjectNotFoundException) {
      if (!invalidUser_)
	panel_->addWidget(invalidUser_ = new WTemplate(tr("blog-invaliduser")));
      panel_->setCurrentWidget(invalidUser_);
    }
  }

  bool checkLoggedIn()
  {
    if (session_.user()) return true;
    panel_->show();
    if (!mustLoginWarning_)
      panel_->addWidget(mustLoginWarning_ = new WTemplate(tr("blog-mustlogin")));
    panel_->setCurrentWidget(mustLoginWarning_);
    return false;
  }

  bool checkAdministrator()
  {
    if (session_.user() && (session_.user()->role == User::Admin)) return true;
    panel_->show();
    if (!mustBeAdministratorWarning_)
      panel_->addWidget(mustBeAdministratorWarning_ = new WTemplate(tr("blog-mustbeadministrator")));
    panel_->setCurrentWidget(mustBeAdministratorWarning_);
    return false;
  }

  Wt::Dbo::ptr<User> findUser(const std::string& name) {
    return session_.find<User>("where name = ?").bind(name);
  }
  
  bool yearMonthDiffer(const WDateTime& dt1, const WDateTime& dt2) {
    return dt1.date().year() != dt2.date().year()
      || dt1.date().month() != dt2.date().month();
  }

  void showArchive(WContainerWidget *parent) {
    static const char* dateFormat = "MMMM yyyy";
    
    new WText(tr("archive-title"), parent);

    Posts posts = session_.find<Post>("order by date desc");

    WDateTime formerDate;
    for (Posts::const_iterator i = posts.begin(); i != posts.end(); ++i) {
      if ((*i)->state != Post::Published)
	continue;

      if (formerDate.isNull() 
	  || yearMonthDiffer(formerDate, (*i)->date)) {
	WText *title
	  = new WText((*i)->date.date().toString(dateFormat), parent);
	title->setStyleClass("archive-month-title");
      }
      
      WAnchor *a = new WAnchor("", parent);
      a->setText((*i)->title);
      a->setRefInternalPath(basePath_ + (*i)->permaLink());
      a->setInline(false);
      
      formerDate = (*i)->date;
    }
  }

  void showPostsByDateTopic(const std::string& path,
			    WContainerWidget *parent) {
    std::vector<std::string> parts;
    boost::split(parts, path, boost::is_any_of("/"));

    WDate lower, upper;
    int year = boost::lexical_cast<int>(parts[0]);

    if (parts.size() > 1) {
      int month = boost::lexical_cast<int>(parts[1]);

      if (parts.size() > 2) {
	int day = boost::lexical_cast<int>(parts[2]);

	lower.setDate(year, month, day);
	upper = lower.addDays(1);
      } else {
	lower.setDate(year, month, 1);
	upper = lower.addMonths(1);
      }
    } else {
      lower.setDate(year, 1, 1);
      upper = lower.addYears(1);
    }

    Posts posts = session_.find<Post>
      ("where date >= ? "
       "and date < ? "
       "and (state = ? or author_id = ?)")
      .bind(WDateTime(lower))
      .bind(WDateTime(upper))
      .bind(Post::Published)
      .bind(session_.user().id());

    if (parts.size() > 3) {
      std::string title = parts[3];

      for (Posts::const_iterator i = posts.begin(); i != posts.end(); ++i)
	if ((*i)->titleToUrl() == title) {
	  showPost(*i, PostView::Detail, parent);
	  return;
	}

      showError(tr("blog-no-post"));
    } else {
      showPosts(posts, parent);
    }
  }

  void showPosts(Wt::Dbo::ptr<User> user) {
    /*
<<<<<<< HEAD:examples/blog/view/BlogView.C
    if (user == session_.user() && user->role == User::Admin) {
      WTemplate *authorPanel = new WTemplate(tr("blog-author-panel"), items_);

      WPushButton *newPost = new WPushButton(tr("new-post"));
      newPost->clicked().connect(this, &BlogImpl::newPost);

      WContainerWidget *unpublishedPosts = new WContainerWidget();
      showPosts(user->allPosts(Post::Unpublished), unpublishedPosts);

      authorPanel->bindString("user", user->name);
      authorPanel->bindInt("unpublished-count",
			   user->allPosts(Post::Unpublished).size());
      authorPanel->bindInt("published-count",
			   user->allPosts(Post::Published).size());
      authorPanel->bindWidget("new-post", newPost);
      authorPanel->bindWidget("unpublished-posts", unpublishedPosts);
    }

=======
>>>>>>> bvh_blog:examples/blog/view/BlogView.C
    */
    showPosts(user->latestPosts(), items_);
  }

  void newPost() {
    Wt::Dbo::Transaction t(session_);

	authorPanel();
	WContainerWidget *unpublishedPosts
      = authorPanel_->resolve<WContainerWidget *>("unpublished-posts");

    Wt::Dbo::ptr<Post> post(new Post);

    Post *p = post.modify();
    p->state = Post::Unpublished;
    p->author = session_.user();
    p->title = "Title";
    p->briefSrc = "Brief ...";
    p->bodySrc = "Body ...";

    showPost(post, PostView::Edit, unpublishedPosts);

    t.commit();
  }

  void showPosts(const Posts& posts, WContainerWidget *parent) {
    for (Posts::const_iterator i = posts.begin(); i != posts.end(); ++i)
      showPost(*i, PostView::Brief, parent);
  }

  void showPost(const Wt::Dbo::ptr<Post> post, PostView::RenderType type,
		WContainerWidget *parent) {
    parent->addWidget(new PostView(session_, basePath_, post, type));
  }

  void showError(const WString& msg) {
    items_->addWidget(new WText(msg));
  }
};
#endif

Morda::Morda(const std::string& base_path, const std::string& sqlite_db, Wt::WContainerWidget* parent)
  : Wt::WCompositeWidget(parent)
  , user_changed_(this) {
  impl_ = new Site(base_path, sqlite_db, this);
  setImplementation(impl_);
}

Wt::WString Morda::User() {
  if (impl_->session_.GetUser()) {
    return impl_->session_.GetUser()->name_;
  } else {
    return Wt::WString::Empty;
  }
}

