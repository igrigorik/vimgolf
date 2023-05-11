module OmniAuthHelper
  def mock_omni_auth
    OmniAuth.config.test_mode = true

    OmniAuth.config.mock_auth[:twitter] = OmniAuth::AuthHash.new(
      {
        provider: "twitter",
        uid: "123545",
        info: {
          name: "bill nye",
          nickname: "the science guy",
          image: "foo.jpg"
        }
      }
    )
  end

  def mock_omni_auth_github
    OmniAuth.config.test_mode = true

    OmniAuth.config.mock_auth[:github] = OmniAuth::AuthHash.new(
      {
        provider: "github",
        uid: "521343",
        info: {
          name: "Linus Torvalds",
          nickname: "the god coder",
          image: "bar.jpg"
        }
      }
    )
  end

  def mock_omni_auth_participator
    OmniAuth.config.test_mode = true

    OmniAuth.config.mock_auth[:twitter] = OmniAuth::AuthHash.new(
      {
        provider: "twitter",
        uid: "1235456",
        info: {
          name: "zelda",
          nickname: "Z",
          image: "foo.jpg"
        }
      }
    )
  end
end
