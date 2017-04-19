module Rails
  module Mongoid
    class << self
      # We need to monkey patch this because the way the current mongoig gem passes
      # paths to `require_dependecy` is incorrect.
      # I'm taking the implementation from mongoid v2.2.1 for right now,
      # it can be removed when we bump mongo and related gems.
      def load_models(app)
        app.config.paths["app/models"].each do |path|
          Dir.glob("#{path}/**/*.rb").sort.each do |file|
            require_dependency(file.gsub("#{path}/" , "").gsub(".rb", ""))
          end
        end
      end
    end
  end
end
